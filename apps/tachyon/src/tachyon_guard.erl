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
-export([start_link/1, put/6, stop/1, stats/0, stats/1]).

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
          dist = 0.0,
          warn = 0,
          info = 0,
          error = 0
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
      {local, server_name(IP)},
      ?MODULE, [IP], []).

stop(IP) ->
    gen_server:cast(server_name(IP), stop).

put(IP, Host, Time, Metric, Value, T) ->
    gen_server:cast(server_name(IP), {put, Host, Time, Metric, Value, T}).

stats(IP) ->
    gen_server:call(server_name(IP), stats).

stats() ->
    case application:get_env(tachyon, clients) of
        {ok, IPs} ->
            [stats(IP) || IP <- IPs],
            ok;
        _ ->
            ok
    end.

server_name(IP) ->
    list_to_atom(IP ++ "_guard").
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

handle_call(stats, _, State = #state{metrics = Metrics} ) ->
    io:format("===================="
              "===================="
              "===================="
              "===================="
              "===~n"
              "~45s~n"
              "===================="
              "===================="
              "===================="
              "===================="
              "===~n",
              [State#state.ip]),
    io:format("~20s ~20s ~20s ~s~n", ["Metric", "Average",
                                      "Standard Derivation", "Itterations"]),
    io:format("~20s ~20s ~20s ~s~n", ["--------------------",
                                 "--------------------",
                                 "--------------------",
                                 "--------------------"]),
    [io:format("~20s ~20.2f ~20.2f ~p~n", [N, M#metric.avg, M#metric.std, M#metric.itteration]) ||
        {N, M} <- Metrics],
    Dist = distance(State),
    {I, W, E} = lists:foldl(fun ({_, #metric{info = I0, warn = W0, error = E0}},
                                 {Ia, Wa, Ea}) ->
                                    {Ia + I0, Wa + W0, Ea + E0}
                            end, {0, 0, 0}, Metrics),
    io:format("Having a total of ~p infos, ~p warnings and ~p errors.~n",
              [I, W, E]),
    io:format("Total distance is ~.2f.~n", [Dist]),

    io:format("===================="
              "===================="
              "===================="
              "===================="
              "===~n"),
    {reply, ok, State};


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
    Metrics1 =
        orddict:update(M, fun (Met) ->
                                  process_warnings(Host, IP, M,
                                                   update_metric(Met, V), T, V)
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
                  {ok, NewDB} = gen_tcp:connect(?DB_SERVER, ?DB_PORT,
                                                [{packet, line}]),
                  NewDB;
              _ ->
                  DB
          end,
    {noreply, State1#state{db = DB1, time = Time}};

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
                end, 0.0, Metrics).

update_metric(Met, V) ->
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
    Dist = if
               Std > 0 ->
                   abs(D/Std);
               true ->
                   0.0
           end,
    Met#metric{
       var = Var, std = Std, avg = Avg,
       itteration = Iter + 1,
       dist = Dist
      }.

process_warnings(Host, IP, Name,
                 Met = #metric{
                          std = Std, dist = Dist,
                          itteration = Iter,
                          info = Info0, warn = Warn0, error = Error0
                         }, T, V) when Std > 0 ->
    Msg = "[~s(~s)/~s] Value ~.2f is ~.2f(~.2f%) std diviations from avg ~.2f.",
    RelDist = Dist/Std,
    Avg = Met#metric.avg,
    Diff = Avg - V,
    %% We don't ant to have alerts if the total delta is less then 1%
    Delta = abs(Diff/Avg),
    MinDelta = 0.02,
    Args = [Host, IP, Name, V*1.0,
            RelDist, Delta, Avg],
    {Info, Warn, Error} =
        if
            RelDist > T*3,
            Iter > ?MIN_ITER,
            Delta > MinDelta ->
                lager:error(Msg, Args),
                {Info0, Warn0, Error0 + 1};
            RelDist > T*2,
            Iter > ?MIN_ITER,
            Delta > MinDelta ->
                lager:warning(Msg, Args),
                {Info0, Warn0 + 1, 0};
            RelDist > T,
            Iter > ?MIN_ITER,
            Delta > MinDelta ->
                lager:info(Msg, Args),
                {Info0 + 1, 0, 0};
        true ->
            {0, 0, 0}
    end,
    Met#metric{info = Info, warn = Warn, error = Error};

process_warnings(_, _, _, Met, _, _) ->
    Met.
