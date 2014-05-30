%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_guard).

-include("tachyon_statistics.hrl").

%% API
-export([init/1, put/5, stats/1, start/1]).
-ignore_xref([start/1]).

-define(SERVER, ?MODULE).
-define(GUARD, true).

-define(DB_SERVER, "172.21.0.203").
-define(DB_PORT, 4242).

-record(state, {metrics = gb_trees:empty(), db, host, time}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start(Host) ->
    spawn(tachyon_guard, init, [Host]).

-ifdef(GUARD).
put(Host, Time, Metric, Value, T) ->
    tproc:where({tachyon_guard, Host}) ! {put, Host, Time, Metric, Value, T}.
-else.
put(_Host, _Time, _Metric, _Value, _T) ->
    ok.
-endif.

stats(Host) ->
    gen_server:call({global, {guard, Host}}, stats).
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
init(Host) ->
    process_flag(trap_exit, true),
    {ok, DB} = tachyon_backend:init(),
    loop(#state{db=DB, host = Host}).

loop(State) ->
    receive

        stats ->
            #state{metrics = Metrics0} = State,
            Metrics = gb_trees:to_list(Metrics0),
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
                      [State#state.host]),
            io:format("~20s ~20s ~20s ~s~n", ["Metric", "Average",
                                              "Standard Derivation", "Itterations"]),
            io:format("~20s ~20s ~20s ~s~n", ["--------------------",
                                              "--------------------",
                                              "--------------------",
                                              "--------------------"]),
            [io:format("~20s ~20.2f ~20.2f ~p~n", [N, M#running_avg.avg, M#running_avg.std, M#running_avg.itteration]) ||
                {N, M} <- Metrics],
            Dist = distance(State),
            {I, W, E} = lists:foldl(fun ({_, #running_avg{info = I0, warn = W0, error = E0}},
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
            loop(State);


        {put, Host, _Time, Name, V, T} ->
            #state{metrics = Metrics,
                   db = _DB,
                   time = _T0} = State,
            Metrics1 = case gb_trees:lookup(Name, Metrics) of
                           none ->
                               gb_trees:insert(Name, #running_avg{avg=V}, Metrics);
                           {value, Met} ->
                               M0 = tachyon_statistics:avg_update(Met, V),
                               {A, M} = tachyon_statistics:avg_analyze(M0, V, T),
                               case A of
                                   {error, Msg, Args} ->
                                       lager:error("[~s/~s] " ++ Msg,
                                                   [Host, Name | Args]);
                                   {warn, Msg, Args} ->
                                       lager:warning("[~s/~s] " ++ Msg,
                                                     [Host, Name | Args]);
                                   {info, Msg, Args} ->
                                       lager:info("[~s/~s] " ++ Msg,
                                                  [Host, Name | Args]);
                                   _ ->
                                       ok
                               end,
                               gb_trees:update(Name, M, Metrics)
                       end,
            %%_Metr = gb_trees:fetch(Name, Metrics1),
            State1  = State#state{metrics = Metrics1},
            %% Msg0 = case Time of
            %%            T0 ->
            %%                [];
            %%            _ ->
            %%                [io_lib:format("put cloud.diff ~p ~p host=~s~n",
            %%                               [T0, distance(State), Host])]
            %%        end,
            %% TelnetMsg = [io_lib:format("put cloud.~s.avg ~p ~p host=~s~n",
            %%                            [Name, Time, Metr#running_avg.avg, Host]),
            %%              io_lib:format("put cloud.~s.std ~p ~p host=~s~n",
            %%                            [Name, Time, Metr#running_avg.std, Host]) | Msg0],
            %% DB1 = case gen_tcp:send(DB, TelnetMsg) of
            %%           {error, Reason} ->
            %%               timer:sleep(1000),
            %%               io:format("[~s] Socket died with: ~p", [Host, Reason]),
            %%               {ok, NewDB} = gen_tcp:connect(?DB_SERVER, ?DB_PORT,
            %%                                             [{packet, line}]),
            %%               NewDB;
            %%           _ ->
            %%               DB
            %%       end,
            loop(State1);
        {'EXIT', _FromPid, _Reason} ->
            ok;
        _ ->
            loop(State)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
distance(#state{metrics = Metrics}) ->
    lists:foldl(fun({_, #running_avg{dist = Dist}}, Acc) ->
                        Acc + Dist
                end, 0.0, Metrics).
