%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_guard).

-behaviour(gen_server).
-include("packet_pb.hrl").

%% API
-export([start_link/1, put/6, stop/1, stats/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(DB_SERVER, "127.0.0.1").
-define(DB_PORT, 4242).


-define(MIN_ITER, 1000).
-define(DECAY, 0.9999).
-define(QUICK_DECAY, 0.99).

-define(UPD(Avg, N, Decay),
        (Avg*Decay) + (N*(1-Decay))).

-record(metric, {
          avg = 0,
          var = 0,
          std = 0,
          itteration = 0,
          dist = 0
         }).


-record(state, {metrics = [], db, ip, time}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(IP) ->
    gen_server:start_link(
      {local, list_to_atom(IP ++ "_guard")},
      ?MODULE, [IP], []).

stop(IP) ->
    gen_server:cast(list_to_atom(IP ++ "_guard"), stop).

put(IP, Host, Time, Metric, Value, T) ->
    gen_server:cast(list_to_atom(IP ++ "_guard"), {put, Host, Time, Metric, Value, T}).

stats(IP) ->
    gen_server:cast(list_to_atom(IP ++ "_guard"), stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([IP]) ->
    process_flag(trap_exit, true),
    {ok, DB} = gen_tcp:connect(?DB_SERVER, ?DB_PORT, [{packet, line}]),
    {ok, #state{db=DB, ip = IP}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({put, Host, Time, M, V, T}, State =
                #state{metrics = Metrics,
                       db = DB,
                       ip=IP,
                       time = T0}) ->
    Metrics1 = orddict:update(M, fun (Met) ->
                                         D = Met#metric.avg - V,
                                         Dsq = D*D,
                                         Iter = Met#metric.itteration,
                                         Var0 = Met#metric.var,
                                         Avg0 = Met#metric.avg,
                                         {Var, Avg} =
                                             case Iter of
                                                 _I0 when _I0 < ?MIN_ITER ->
                                                     {?UPD(Var0, Dsq, ?QUICK_DECAY),
                                                      ?UPD(Avg0, V, ?QUICK_DECAY)};
                                                 _ ->
                                                     {?UPD(Var0, Dsq, ?DECAY),
                                                      ?UPD(Avg0, V, ?DECAY)}
                                             end,
                                         Std = math:sqrt(Var),
                                         Msg = "[~s(~s)/~s] Value ~p is ~p std diviations from avg ~p",
                                         Dist = case {Std, Iter} of
                                                    {_S, _I1} when _S > 0,
                                                                   _I1 > ?MIN_ITER ->

                                                        DistI = abs(D/Std),
                                                        case DistI of
                                                            R when R > T*3 ->
                                                                lager:error(
                                                                  Msg,
                                                                  [Host, IP, M,
                                                                   V, R, Avg]);
                                                            R when R > T*2 ->
                                                                lager:warning(
                                                                  Msg,
                                                                  [Host, IP, M,
                                                                   V, R, Avg]);
                                                            R when R > T ->
                                                                lager:info(
                                                                  Msg,
                                                                  [Host, IP, M,
                                                                   V, R, Avg]);
                                                            _ ->
                                                                ok
                                                        end,
                                                        DistI;
                                                    _ ->
                                                        0
                                                end,
                                         #metric{
                                            var = Var,
                                            std = Std,
                                            avg = Avg,
                                            itteration = Iter + 1,
                                            dist = Dist
                                           }
                                 end, #metric{avg=V}, Metrics),
    DB1 = DB,
    Metr = orddict:fetch(M, Metrics1),
    State1  = State#state{metrics = Metrics1},
    Msg0 = case Time of
               T0 ->
                   [];
               _ ->
                   [io_lib:format("put cloud.diff ~p ~p host=~s~n",
                                  [T0, distance(State), Host])]
           end,
    TelnetMsg = [io_lib:format("put cloud.~s.avg ~p ~p host=~s~n",
                               [M, Time, Metr#metric.avg, Host]),
                 io_lib:format("put cloud.~s.std ~p ~p host=~s~n",
                               [M, Time, Metr#metric.std, Host]) | Msg0],
    DB1 = case gen_tcp:send(DB, TelnetMsg) of
              {error, Reason} ->
                  timer:sleep(1000),
                  io:format("[~s] Socket died with: ~p", [IP, Reason]),
                  {ok, NewDB} = gen_tcp:connect(?DB_SERVER, ?DB_PORT, [{packet, line}]),
                  NewDB;
              _ ->
                  DB
          end,
    {noreply, State1#state{db = DB1, time = Time}};

handle_cast(stats, State = #state{metrics = Metrics} ) ->
    io:format("~20s ~20s ~s~n", ["Metric", "Average", "Standard Derivation"]),
    io:format("~20s ~20s ~s~n", ["--------------------",
                                 "--------------------",
                                 "--------------------"]),
    [io:format("~20s ~20p ~20p~n", [N, M#metric.avg, M#metric.std]) ||
        {N, M} <- Metrics],
    Dist = distance(State),
    io:format("Total distance is ~p.~n", [Dist]),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.db),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
distance(#state{metrics = Metrics}) ->
    lists:foldl(fun({_, #metric{dist = Dist}}, Acc) ->
                        Acc + Dist
                end, 0, Metrics).
