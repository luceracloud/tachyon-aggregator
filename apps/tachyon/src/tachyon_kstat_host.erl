%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_kstat_host).

%% API
-export([init/1, start/1]).
-ignore_xref([start/1]).

-record(state, {host, db}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> Pid
%% @end
%%--------------------------------------------------------------------

start(Host) ->
    spawn(tachyon_kstat_host, init, [Host]).

%%%===================================================================
%%% Interal Code
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
    {ok, DB} = tachyon_kairos:connect(),
    {ok, Statsd} = tachyon_statsd:connect(),
    loop(#state{host = Host, db=[{tachyon_kairos, DB}, {tachyon_statsd, Statsd}]}).

loop(State) ->
    receive
        {Host, _, SnapTime, {<<"ip">>, _Instance, _Name, _Class}, {Key, V}} ->
            tachyon_guard:put(Host, SnapTime, <<"ip.", Key/binary>>, V, 4),
            loop(put(<<"cloud.host.ip.", Key/binary>>, V, SnapTime,
                     [{<<"host">>, Host}], State));

        {Host, _, SnapTime, {<<"zfs">>, _Instance, <<"arcstat">>, _Class}, {Key, V}} ->
            Metric = <<"arc.", Key/binary>>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 4),
            loop(put(<<"cloud.host.arc.", Key/binary>>, V, SnapTime,
                     [{<<"host">>, Host}], State));

        {Host, _, SnapTime,
         {<<"sd">>, Instance, _Name, _Class}, {Key, V}} ->

            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].", Key/binary>>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 4),
            loop(put(<<"cloud.host.disk.metrics.", Key/binary>>, V, SnapTime,
                     [{<<"host">>, Host}, {<<"disk">>, Instance}], State));

        {Host, _, SnapTime,
         {<<"sderr">>, Instance, _Name, _Class}, {<<"Hard Errors">>, V}} ->

            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].errors.hard">>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 1),
            loop(put(<<"cloud.host.disk.errors.hard">>, V, SnapTime,
                     [{<<"host">>, Host}, {<<"disk">>, Instance}], State));

        {Host, _, SnapTime,
         {<<"sderr">>, Instance, _Name, _Class}, {<<"Soft Errors">>, V}} ->

            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].errors.hard">>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 1),
            loop(put(<<"cloud.host.disk.errors.soft">>, V, SnapTime,
                     [{<<"host">>, Host}, {<<"disk">>, Instance}], State));

        {Host, _, SnapTime,
         {<<"sderr">>, Instance, _Name, _Class}, {<<"Transport Errors">>, V}} ->

            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].errors.transport">>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 1),
            loop(put(<<"cloud.host.disk.errors.transport">>, V, SnapTime,
                     [{<<"host">>, Host}, {<<"disk">>, Instance}], State));

        {Host, _, SnapTime,
         {<<"sderr">>, Instance, _Name, _Class}, {<<"Illegal Request">>, V}} ->

            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].", "illegal_requests">>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 1),
            loop(put(<<"cloud.host.disk.errors.illegal">>, V, SnapTime,
                     [{<<"host">>, Host}, {<<"disk">>, Instance}], State));

        {Host, _, SnapTime,
         {<<"sderr">>, Instance, _Name, _Class}, {<<"Predictive Failure Analysis">>, V}} ->

            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].", "predicted_failures">>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 1),
            loop(put(<<"cloud.host.disk.errors.predicted_failures">>, V, SnapTime,
                     [{<<"host">>, Host}, {<<"disk">>, Instance}], State));

        %% CPU Load
        {Host, _, SnapTime,
         {<<"cpu_stat">>, Instance, _Name, _Class}, {Key, V}} ->

            ID = list_to_binary(integer_to_list(Instance)),
            tachyon_guard:put(Host, SnapTime, <<"cpu[", ID/binary, "].", Key/binary>>, V, 4),
            loop(put(<<"cloud.host.cpu.", Key/binary>>, V, SnapTime,
                     [{<<"host">>, Host}, {<<"cpu">>, Instance}], State));

        %% IP_NIC_EVENT_QUEUE
        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"vnd_str_cache">>, _Class}, {Key, V}} ->

            loop(put(<<"cloud.host.cache.streams.vnd.", Key/binary>>, V, SnapTime,
                     [{<<"host">>, Host}], State));

        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"dld_str_cache">>, _Class}, {Key, V}} ->

            loop(put(<<"cloud.host.cache.streams.vnd.", Key/binary>>, V, SnapTime,
                     [{<<"host">>, Host}], State));

        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"udp_conn_cache">>, _Class}, {Key, V}} ->
            loop(put(<<"cloud.host.cache.connections.udp.", Key/binary>>, V, SnapTime,
                     [{<<"host">>, Host}], State));

        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"tcp_conn_cache">>, _Class}, {Key, V}} ->
            loop(put(<<"cloud.host.cache.connections.tcp.", Key/binary>>, V, SnapTime,
                     [{<<"host">>, Host}], State));

        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"socket_cache">>, _Class}, {Key, V}} ->
            loop(put(<<"cloud.host.cache.socket.", Key/binary>>, V, SnapTime,
                     [{<<"host">>, Host}], State));

        %% Netork Caches
        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"IP_NIC_EVENT_QUEUE">>, _Class}, {Key, V}} ->
            loop(put(<<"cloud.host.ip_nic_event_queue.", Key/binary>>, V, SnapTime,
                     [{<<"host">>, Host}], State));
        {'EXIT', _, _} ->
            ok;
        _ ->
            loop(State)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

put(Metric, Value, Time, Args, State = #state{db = DBs}) ->
    DBs1 = [{Mod, Mod:put(Metric, Value, Time, Args, DB)} ||
               {Mod, DB} <- DBs],
    State#state{db = DBs1}.
