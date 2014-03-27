%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_kstat_zone).

-behaviour(gen_server).

%% API
-export([start_link/1, msg/1]).
-ignore_xref([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(DB_SERVER, "172.21.0.203").
-define(DB_PORT, 4242).

-record(state, {host, db}).

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
start_link(Zone) ->
    gen_server:start_link({global, {zone, Zone}}, ?MODULE, [], []).

msg({_, Zone, _, _, _} = P) ->
    case global:whereis_name({zone, Zone}) of
        undefined ->
            {ok, Pid} = tachyon_kstat_zone_sup:start_child(Zone),
            gen_server:cast(Pid, P);
        Pid ->
            gen_server:cast(Pid, P)
    end.

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
init([]) ->
    process_flag(trap_exit, true),
    {ok, DB} = tachyon_kairos:connect({?DB_SERVER, ?DB_PORT}),
    {ok, #state{db=DB}}.

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

handle_cast(M, State) ->
    case process_info(self(), message_queue_len) of
        {message_queue_len,_N} when _N < 10000 ->
            tachyon_mps:handle(),
            handle_zone(M, State);
        _ -> {stop, overflow, State}
    end.

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
terminate(_Reason, _State) ->
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

put(Metric, Value, Time, Args, State = #state{db = DB}) ->
    DB1 = tachyon_kairos:put(Metric, Value, Time, Args, DB),
    State#state{db = DB1}.

handle_zone({Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"above_base_sec">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.above_base">>, V, SnapTime,
                 [{host, Host}, {zone, Zone}], State),
    {noreply, State1};

handle_zone({Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"above_sec">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.above">>, V, SnapTime,
                 [{host, Host}, {zone, Zone}], State),
    {noreply, State1};

handle_zone({Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"baseline">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.baseline">>, V, SnapTime,
                 [{host, Host}, {zone, Zone}], State),
    {noreply, State1};

handle_zone({Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"burst_limit_sec">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.burst_limit">>, V, SnapTime,
                 [{host, Host}, {zone, Zone}], State),
    {noreply, State1};

handle_zone({Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"burst_sec">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.burst">>, V, SnapTime,
                 [{host, Host}, {zone, Zone}], State),
    {noreply, State1};

handle_zone({Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"effective">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.effective">>, V, SnapTime,
                 [{host, Host}, {zone, Zone}], State),
    {noreply, State1};

handle_zone({Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"maxusage">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.maxusage">>, V, SnapTime,
                 [{host, Host}, {zone, Zone}], State),
    {noreply, State1};

handle_zone({Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"nwait">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.nwait">>, V, SnapTime,
                 [{host, Host}, {zone, Zone}], State),
    {noreply, State1};

handle_zone({Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"usage">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.usage">>, V, SnapTime,
                 [{host, Host}, {zone, Zone}], State),
    {noreply, State1};

handle_zone({Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"value">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.value">>, V, SnapTime,
                 [{host, Host}, {zone, Zone}], State),
    {noreply, State1};

handle_zone(
  {Host, Zone, SnapTime, {<<"caps">>=Module, Instance, Name, Class}, {Key, V}},
  State) ->
    lager:debug("[~s:~s@~p] "
                "~s:~p:~s(~s) "
                "~s = ~p~n",
                [Host, Zone, SnapTime,
                 Module, Instance, Class, Name,
                 Key, V]),
      {noreply, State};



handle_zone({Host, Zone, SnapTime, {Module, Instance, Name, Class}, {Key, V}}, State) ->
    lager:debug("[~s:~s@~p] "
                "~s:~p:~s(~s) "
                "~s = ~p~n",
                [Host, Zone, SnapTime,
                 Module, Instance, Class, Name,
                 Key, V]),
    {noreply, State}.
