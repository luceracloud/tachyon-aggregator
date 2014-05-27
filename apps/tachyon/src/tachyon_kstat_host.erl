%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_kstat_host).

-behaviour(gen_server).

%% API
-export([start_link/1, start/1]).
-ignore_xref([start_link/1, start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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
start_link(Host) ->
    gen_server:start_link({global, {host, Host}}, ?MODULE, [Host], []).

start(Host) ->
    tachyon_kstat_host_sup:start_child(Host).

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
    {ok, DB} = tachyon_kairos:connect(),
    {ok, Statsd} = tachyon_statsd:connect(),
    {ok, #state{host = Host, db=[{tachyon_kairos, DB}, {tachyon_statsd, Statsd}]}}.

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
            handle_gz(M, State);
        _ ->
            {noreply,  State}
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
handle_info({_, _, _, _, _} = M, State) ->
    handle_gz(M, State);

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

put(Metric, Value, Time, Args, State = #state{db = DBs}) ->
    DBs1 = [{Mod, Mod:put(Metric, Value, Time, Args, DB)} ||
               {Mod, DB} <- DBs],
    State#state{db = DBs1}.

handle_gz({Host, _, SnapTime,
           {<<"ip">>, _Instance, _Name, _Class}, {Key, V}},
          State) ->
    
    tachyon_guard:put(Host, SnapTime, <<"ip.", Key/binary>>, V, 4),
    State1 = put(<<"cloud.host.ip.", Key/binary>>, V, SnapTime,
                 [{<<"host">>, Host}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"zfs">>, _Instance, <<"arcstat">>, _Class}, {Key, V}},
          State) ->
    Metric = <<"arc.", Key/binary>>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 4),
    State1 = put(<<"cloud.host.arc.", Key/binary>>, V, SnapTime,
                 [{<<"host">>, Host}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"sd">>, Instance, _Name, _Class}, {Key, V}},
          State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", Key/binary>>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 4),
    State1 = put(<<"cloud.host.disk.metrics.", Key/binary>>, V, SnapTime,
                 [{<<"host">>, Host}, {<<"disk">>, Instance}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"sderr">>, Instance, _Name, _Class}, {<<"Hard Errors">>, V}},
          State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].errors.hard">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    State1 = put(<<"cloud.host.disk.errors.hard">>, V, SnapTime,
                 [{<<"host">>, Host}, {<<"disk">>, Instance}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"sderr">>, Instance, _Name, _Class}, {<<"Soft Errors">>, V}},
          State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].errors.hard">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    State1 = put(<<"cloud.host.disk.errors.soft">>, V, SnapTime,
                 [{<<"host">>, Host}, {<<"disk">>, Instance}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"sderr">>, Instance, _Name, _Class}, {<<"Transport Errors">>, V}},
          State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].errors.transport">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    State1 = put(<<"cloud.host.disk.errors.transport">>, V, SnapTime,
                 [{<<"host">>, Host}, {<<"disk">>, Instance}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"sderr">>, Instance, _Name, _Class}, {<<"Illegal Request">>, V}},
          State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "illegal_requests">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    State1 = put(<<"cloud.host.disk.errors.illegal">>, V, SnapTime,
                 [{<<"host">>, Host}, {<<"disk">>, Instance}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"sderr">>, Instance, _Name, _Class}, {<<"Predictive Failure Analysis">>, V}},
          State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "predicted_failures">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    State1 = put(<<"cloud.host.disk.errors.predicted_failures">>, V, SnapTime,
                 [{<<"host">>, Host}, {<<"disk">>, Instance}], State),
    {noreply, State1};

%% CPU Load
handle_gz({Host, _, SnapTime,
           {<<"cpu_stat">>, Instance, _Name, _Class}, {Key, V}},
          State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    tachyon_guard:put(Host, SnapTime, <<"cpu[", ID/binary, "].", Key/binary>>, V, 4),
    State1 = put(<<"cloud.host.cpu.", Key/binary>>, V, SnapTime,
                 [{<<"host">>, Host}, {<<"cpu">>, Instance}], State),
    {noreply, State1};

%% IP_NIC_EVENT_QUEUE
handle_gz({Host, _, SnapTime,
           {<<"unix">>, _Instance, <<"vnd_str_cache">>, _Class}, {Key, V}},
          State) ->
    State1 = put(<<"cloud.host.cache.streams.vnd.", Key/binary>>, V, SnapTime,
                 [{<<"host">>, Host}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"unix">>, _Instance, <<"dld_str_cache">>, _Class}, {Key, V}},
          State) ->
    State1 = put(<<"cloud.host.cache.streams.vnd.", Key/binary>>, V, SnapTime,
                 [{<<"host">>, Host}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"unix">>, _Instance, <<"udp_conn_cache">>, _Class}, {Key, V}},
          State) ->
    State1 = put(<<"cloud.host.cache.connections.udp.", Key/binary>>, V, SnapTime,
                 [{<<"host">>, Host}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"unix">>, _Instance, <<"tcp_conn_cache">>, _Class}, {Key, V}},
          State) ->
    State1 = put(<<"cloud.host.cache.connections.tcp.", Key/binary>>, V, SnapTime,
                 [{<<"host">>, Host}], State),
    {noreply, State1};

handle_gz({Host, _, SnapTime,
           {<<"unix">>, _Instance, <<"socket_cache">>, _Class}, {Key, V}},
          State) ->
    State1 = put(<<"cloud.host.cache.socket.", Key/binary>>, V, SnapTime,
                 [{<<"host">>, Host}], State),
    {noreply, State1};

%% Netork Caches
handle_gz({Host, _, SnapTime,
           {<<"unix">>, _Instance, <<"IP_NIC_EVENT_QUEUE">>, _Class}, {Key, V}},
          State) ->
    State1 = put(<<"cloud.host.ip_nic_event_queue.", Key/binary>>, V, SnapTime,
                 [{<<"host">>, Host}], State),
    {noreply, State1};

handle_gz(_, State) ->
    {noreply, State}.
