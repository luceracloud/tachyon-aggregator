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

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          2:32/integer, "ip",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    put(<<"cloud.host.ip.", Key/binary>>, V, SnapTime,
        [{<<"host">>, Host}], State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          7:32/integer, "arcstat",
          3:32/integer, "zfs",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    Metric = <<"arc.", Key/binary>>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 4),
    put(<<"cloud.host.arc.", Key/binary>>, V, SnapTime,
        [{<<"host">>, Host}], State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          2:32/integer, "sd",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", Key/binary>>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 4),
    put(<<"cloud.host.disk.metrics.", Key/binary>>, V, SnapTime,
        [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          5:32/integer, "sderr",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          11:32/integer, "Hard Errors",
          $i, V:64/integer>>, _, State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].errors.hard">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    put(<<"cloud.host.disk.errors.hard">>, V, SnapTime,
        [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          5:32/integer, "sderr",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          11:32/integer, "Soft Errors",
          $i, V:64/integer>>, _, State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].errors.soft">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    put(<<"cloud.host.disk.errors.soft">>, V, SnapTime,
        [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          5:32/integer, "sderr",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          16:32/integer, "Transport Errors",
          $i, V:64/integer>>, _, State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].errors.transport">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    put(<<"cloud.host.disk.errors.transport">>, V, SnapTime,
        [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          5:32/integer, "sderr",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          27:32/integer, "Predictive Failure Analysis",
          $i, V:64/integer>>, _, State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "predicted_failures">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    put(<<"cloud.host.disk.errors.predicted_failures">>, V, SnapTime,
        [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          5:32/integer, "sderr",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          15:32/integer, "Illegal Request",
          $i, V:64/integer>>, _, State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "illegal_requests">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    put(<<"cloud.host.disk.errors.illegal">>, V, SnapTime,
        [{<<"host">>, Host}, {<<"disk">>, Instance}], State);

%%
%% CPU Load
%%
message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          7:32/integer, "cpu_stat",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    tachyon_guard:put(Host, SnapTime, <<"cpu[", ID/binary, "].", Key/binary>>, V, 4),
    put(<<"cloud.host.cpu.", Key/binary>>, V, SnapTime,
        [{<<"host">>, Host}, {<<"cpu">>, Instance}], State);

%%
%% IP_NIC_EVENT_QUEUE
%%
message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          18:32/integer, "IP_NIC_EVENT_QUEUE",
          4:32/integer, "unix",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    put(<<"cloud.host.ip_nic_event_queue.", Key/binary>>, V, SnapTime,
        [{<<"host">>, Host}], State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, Name:_NameSize/binary,
          4:32/integer, "unix",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    put(<<"cloud.host.cache.", Name/binary, $., Key/binary>>, V, SnapTime,
        [{<<"host">>, Host}], State);

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          6:32/integer, "global",
          _/binary>>, _, State) ->
    State;

message(Msg, _, State) ->
    tachyon_mps:provide(),
    case tachyon_kstat_pkg:decode(Msg) of
        {ok, {_, <<"global">>, _, _, _}} ->
            {ok, State};
        {ok, P} ->
            do_zone(P, State);
        E ->
            lager:error("[msg] ~p", [E]),
            {ok, State}
    end.

do_zone(Msg, State) ->
    case Msg of
        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"above_base_sec">>, V}} ->
            put(<<"cloud.zones.cpu.above_base">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"above_sec">>, V}} ->
            put(<<"cloud.zones.cpu.above">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"baseline">>, V}} ->
            put(<<"cloud.zones.cpu.baseline">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"burst_limit_sec">>, V}} ->
            put(<<"cloud.zones.cpu.burst_limit">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"burst_sec">>, V}} ->
            put(<<"cloud.zones.cpu.burst">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"effective">>, V}} ->
            put(<<"cloud.zones.cpu.effective">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"maxusage">>, V}} ->
            put(<<"cloud.zones.cpu.maxusage">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"nwait">>, V}} ->
            put(<<"cloud.zones.cpu.nwait">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"usage">>, V}} ->
            put(<<"cloud.zones.cpu.usage">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"value">>, V}} ->
            put(<<"cloud.zones.cpu.value">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        %% Memory
        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"physicalmem_zone_", _/binary>>, _},
         {<<"usage">>, V}} ->
            put(<<"cloud.zones.mem.usage">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"physicalmem_zone_", _/binary>>, _},
         {<<"value">>, V}} ->
            put(<<"cloud.zones.mem.value">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        %% Swap
        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"swapresv_zone_", _/binary>>, _},
         {<<"usage">>, V}} ->
            put(<<"cloud.zones.swap.usage">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"swapresv_zone_", _/binary>>, _},
         {<<"value">>, V}} ->
            put(<<"cloud.zones.swap.value">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        %% Procs
        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"nprocs_zone_", _/binary>>, _},
         {<<"usage">>, V}} ->
            put(<<"cloud.zones.procs.usage">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"nprocs_zone_", _/binary>>, _},
         {<<"value">>, V}} ->
            put(<<"cloud.zones.procs.value">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        %% Net
        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"brdcstrcv">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.brdcstrcv">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"brdcstxmt">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.brdcstxmt">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"collisions">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.collisions">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"ierrors">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.ierrors">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"ipackets64">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.ipackets">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"multircv">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.multircv">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"multixmt">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.multixmt">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"norcvbuf">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.norcvbuf">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"noxmtbuf">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.noxmtbuf">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"obytes64">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.obytes">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"oerrors">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.oerrors">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"opackets64">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.opackets">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"rbytes64">>, V}} ->
            IFace = parse_iface(IFInstance),
            put(<<"cloud.zones.net.rbytes">>, V, SnapTime,
                [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

        %% VFS
        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"100ms_ops">>, V}} ->
            put(<<"cloud.zones.vfs.ops.100ms">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"10ms_ops">>, V}} ->
            put(<<"cloud.zones.vfs.ops.10ms">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"10s_ops">>, V}} ->
            put(<<"cloud.zones.vfs.ops.10s">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"1s_ops">>, V}} ->
            put(<<"cloud.zones.vfs.ops.1s">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"delay_cnt">>, V}} ->
            put(<<"cloud.zones.vfs.delay_cnt">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"delay_time">>, V}} ->
            put(<<"cloud.zones.vfs.delay_time">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"nread">>, V}} ->
            put(<<"cloud.zones.vfs.read.n">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"reads">>, V}} ->
            put(<<"cloud.zones.vfs.read.count">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"rlentime">>, V}} ->
            put(<<"cloud.zones.vfs.read.lentime">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"rtime">>, V}} ->
            put(<<"cloud.zones.vfs.read.time">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"nwritten">>, V}} ->
            put(<<"cloud.zones.vfs.write.n">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"writes">>, V}} ->
            put(<<"cloud.zones.vfs.write.count">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);


        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"wlentime">>, V}} ->
            put(<<"cloud.zones.vfs.write.lentime">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"wtime">>, V}} ->
            put(<<"cloud.zones.vfs.write.time">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);


        %% ZFS
        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"nread">>, V}} ->
            put(<<"cloud.zones.zfs.read.n">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"reads">>, V}} ->
            put(<<"cloud.zones.zfs.read.count">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);


        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"rlentime">>, V}} ->
            put(<<"cloud.zones.zfs.read.lentime">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"rtime">>, V}} ->
            put(<<"cloud.zones.zfs.read.time">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"nwritten">>, V}} ->
            put(<<"cloud.zones.zfs.write.n">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"writes">>, V}} ->
            put(<<"cloud.zones.zfs.write.count">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);

        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"wlentime">>, V}} ->
            put(<<"cloud.zones.zfs.write.lentime">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);


        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"wtime">>, V}} ->
            put(<<"cloud.zones.zfs.write.time">>, V, SnapTime,
                [{<<"zone">>, Zone}], State);
        _ ->
            {ok, State}
    end.

parse_iface(<<"z", _, $_, IFace/binary>>) ->
    IFace;
parse_iface(<<"z", _, _, $_, IFace/binary>>) ->
    IFace;
parse_iface(O) ->
    parse_iface1(O).

parse_iface1(<<$_, IFace/binary>>) ->
    IFace;
parse_iface1(<<_, Rest/binary>>) ->
    parse_iface(Rest);
parse_iface1(<<>>) ->
    <<"net">>.

put(Metric, Value, Time, Args, State = #state{db = DBs}) ->
    DBs1 = [{Mod, Mod:put(Metric, Value, Time, Args, DB)} ||
               {Mod, DB} <- DBs],
    {ok, State#state{db = DBs1}}.
