-module(tachyon_kstat_nsq).

-behaviour(ensq_channel_behaviour).

-record(state, {db}).

-export([init/0, response/2, message/3, error/2]).

init() ->
    {ok, DB} = tachyon_kairos:connect(),
    {ok, Statsd} = tachyon_statsd:connect(),
    {ok, #state{db=[{tachyon_kairos, DB}, {tachyon_statsd, Statsd}]}}.

response(_Msg, State) ->
    {ok, State}.

error(_Msg, State) ->
    {ok, State}.

message(Msg, _, State) ->
    tachyon_mps:provide(),
    case tachyon_kstat_pkg:decode(Msg) of
        {ok, {_, <<"global">>, _, _, _} = P} ->
            {ok, do_host(P, State)};
        {ok, {_, Zone, _, _, _} = P} ->
            tproc:where({tachyon_kstat_zone, Zone}) ! P,
            {ok, State};
        E ->
            lager:error("[msg] ~p", [E]),
            {ok, State}
    end.


do_host(Msg, State) ->
    case Msg of
        {Host, _, SnapTime, {<<"ip">>, _Instance, _Name, _Class}, {Key, V}} ->
            tachyon_guard:put(Host, SnapTime, <<"ip.", Key/binary>>, V, 4),
            put(<<"cloud.host.ip.", Key/binary>>, V, SnapTime,
                [{<<"host">>, Host}], State);

        {Host, _, SnapTime, {<<"zfs">>, _Instance, <<"arcstat">>, _Class}, {Key, V}} ->
            Metric = <<"arc.", Key/binary>>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 4),
            put(<<"cloud.host.arc.", Key/binary>>, V, SnapTime,
                [{<<"host">>, Host}], State);

        {Host, _, SnapTime,
         {<<"sd">>, Instance, _Name, _Class}, {Key, V}} ->
            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].", Key/binary>>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 4),
            put(<<"cloud.host.disk.metrics.", Key/binary>>, V, SnapTime,
                [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

        {Host, _, SnapTime,
         {<<"sderr">>, Instance, _Name, _Class}, {<<"Hard Errors">>, V}} ->
            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].errors.hard">>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 1),
            put(<<"cloud.host.disk.errors.hard">>, V, SnapTime,
                [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

        {Host, _, SnapTime,
         {<<"sderr">>, Instance, _Name, _Class}, {<<"Soft Errors">>, V}} ->
            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].errors.hard">>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 1),
            put(<<"cloud.host.disk.errors.soft">>, V, SnapTime,
                [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

        {Host, _, SnapTime,
         {<<"sderr">>, Instance, _Name, _Class}, {<<"Transport Errors">>, V}} ->
            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].errors.transport">>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 1),
            put(<<"cloud.host.disk.errors.transport">>, V, SnapTime,
                [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

        {Host, _, SnapTime,
         {<<"sderr">>, Instance, _Name, _Class}, {<<"Illegal Request">>, V}} ->
            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].", "illegal_requests">>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 1),
            put(<<"cloud.host.disk.errors.illegal">>, V, SnapTime,
                [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

        {Host, _, SnapTime,
         {<<"sderr">>, Instance, _Name, _Class}, {<<"Predictive Failure Analysis">>, V}} ->
            ID = list_to_binary(integer_to_list(Instance)),
            Metric = <<"sd[", ID/binary, "].", "predicted_failures">>,
            tachyon_guard:put(Host, SnapTime, Metric, V, 1),
            put(<<"cloud.host.disk.errors.predicted_failures">>, V, SnapTime,
                [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

        %% CPU Load
        {Host, _, SnapTime,
         {<<"cpu_stat">>, Instance, _Name, _Class}, {Key, V}} ->
            ID = list_to_binary(integer_to_list(Instance)),
            tachyon_guard:put(Host, SnapTime, <<"cpu[", ID/binary, "].", Key/binary>>, V, 4),
            put(<<"cloud.host.cpu.", Key/binary>>, V, SnapTime,
                [{<<"host">>, Host}, {<<"cpu">>, Instance}], State);

        %% IP_NIC_EVENT_QUEUE
        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"vnd_str_cache">>, _Class}, {Key, V}} ->

            put(<<"cloud.host.cache.streams.vnd.", Key/binary>>, V, SnapTime,
                [{<<"host">>, Host}], State);

        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"dld_str_cache">>, _Class}, {Key, V}} ->

            put(<<"cloud.host.cache.streams.vnd.", Key/binary>>, V, SnapTime,
                [{<<"host">>, Host}], State);

        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"udp_conn_cache">>, _Class}, {Key, V}} ->
            put(<<"cloud.host.cache.connections.udp.", Key/binary>>, V, SnapTime,
                [{<<"host">>, Host}], State);

        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"tcp_conn_cache">>, _Class}, {Key, V}} ->
            put(<<"cloud.host.cache.connections.tcp.", Key/binary>>, V, SnapTime,
                [{<<"host">>, Host}], State);

        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"socket_cache">>, _Class}, {Key, V}} ->
            put(<<"cloud.host.cache.socket.", Key/binary>>, V, SnapTime,
                [{<<"host">>, Host}], State);

        %% Netork Caches
        {Host, _, SnapTime,
         {<<"unix">>, _Instance, <<"IP_NIC_EVENT_QUEUE">>, _Class}, {Key, V}} ->
            put(<<"cloud.host.ip_nic_event_queue.", Key/binary>>, V, SnapTime,
                [{<<"host">>, Host}], State);
        _ ->
            ok
    end.

put(Metric, Value, Time, Args, State = #state{db = DBs}) ->
    DBs1 = [{Mod, Mod:put(Metric, Value, Time, Args, DB)} ||
               {Mod, DB} <- DBs],
    State#state{db = DBs1}.
