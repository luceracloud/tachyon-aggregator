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

-define(UPD(Avg, N),
    (Avg*?DECAY) + (N*(1-?DECAY))).

-record(metric, {
          avg = 0,
          var = 0,
          std = 0,
          itteration = 0
         }).


-record(state, {metrics = [], db, ip}).

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
                       ip=IP}) ->
    Metrics1 = orddict:update(M, fun (Met) ->
                                         D = Met#metric.avg - V,
                                         Dsq = D*D,
                                         Var = ?UPD(Met#metric.var, Dsq),
                                         Avg = ?UPD(Met#metric.avg, V),
                                         Std = math:sqrt(Var),
                                         Iter = Met#metric.itteration,
                                         case {Std, Iter} of
                                             {_S, I} when _S =/= 0,
                                                          I > ?MIN_ITER ->
                                                 case abs(D/Std) of
                                                     R when R > T ->
                                                         io:format("[~s] Standard Derivatin >~p -> ~p.~n", [M, T, R]);
                                                     _ ->
                                                         ok
                                                 end;
                                             _ ->
                                                 ok
                                         end,
                                         #metric{
                                            var = Var,
                                            std = Std,
                                            avg = Avg,
                                            itteration = Iter + 1
                                         }
                                 end, #metric{avg=V}, Metrics),
    DB1 = DB,
    Metr = orddict:fetch(M, Metrics1),

    TelnetMsg = [io_lib:format("put machines.~s.avg ~p ~p host=~s~n",
                               [M, Time, Metr#metric.avg, Host]),
                 io_lib:format("put machines.~s.std ~p ~p host=~s~n",
                               [M, Time, Metr#metric.std, Host])],
    DB1 = case gen_tcp:send(DB, TelnetMsg) of
               {error, Reason} ->
                   timer:sleep(1000),
                   io:format("[~s] Socket died with: ~p", [IP, Reason]),
                   {ok, NewDB} = gen_tcp:connect(?DB_SERVER, ?DB_PORT, [{packet, line}]),
                   NewDB;
               _ ->
                  DB
           end,
    {noreply, State#state{db = DB1, metrics = Metrics1}};

handle_cast(stats, State) ->
    io:format("~20s ~20s ~s~n", ["Metric", "Average", "Standard Derivation"]),
    io:format("~20s ~20s ~s~n", ["--------------------",
                                 "--------------------",
                                 "--------------------"]),

    [
     io:format("~20s ~20p ~20p~n", [N, M#metric.avg, M#metric.std]) ||
        {N, M} <- State#state.metrics],
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
