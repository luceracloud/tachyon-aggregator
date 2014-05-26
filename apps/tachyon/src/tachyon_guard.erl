%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_guard).

-behaviour(gen_server).

-include("tachyon_statistics.hrl").

%% API
-export([start_link/1, put/5, stats/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/1]).

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
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Host) ->
    gen_server:start_link(
      {global, {guard, Host}},
      ?MODULE, [Host], []).
-ifdef(GUARD).
put(Host, Time, Metric, Value, T) ->
    case global:whereis_name({guard, Host}) of
        undefined ->
            {ok, Pid} = tachyon_guard_sup:start_child(Host),
            gen_server:cast(Pid, {put, Host, Time, Metric, Value, T});
        Pid ->
            gen_server:cast(Pid, {put, Host, Time, Metric, Value, T})
    end.
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
init([Host]) ->
    process_flag(trap_exit, true),
    {ok, DB} = gen_tcp:connect(?DB_SERVER, ?DB_PORT, [{packet, line}]),
    {ok, #state{db=DB, host = Host}}.

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

handle_call(stats, _, State = #state{metrics = Metrics0} ) ->
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
handle_cast({put, Host, Time, Name, V, T}, State =
                #state{metrics = Metrics,
                       db = _DB,
                       time = _T0}) ->
    Metrics1 =
        gb_trees:update(Name,
                       fun (Met) ->
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
                               M
                          end, #running_avg{avg=V}, Metrics),
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
    {noreply, State1#state{time = Time}};

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
    lists:foldl(fun({_, #running_avg{dist = Dist}}, Acc) ->
                        Acc + Dist
                end, 0.0, Metrics).
