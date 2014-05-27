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

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          _ZoneSize:32/integer, _Zone:_ZoneSize/binary,
          _SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          _ModuleSize:32/integer, _Module:_ModuleSize/binary,
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          6:32/integer, "crtime", _/binary>>, _, State) ->
    {ok, State};

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          _ZoneSize:32/integer, _Zone:_ZoneSize/binary,
          _SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          _ModuleSize:32/integer, _Module:_ModuleSize/binary,
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          8:32/integer, "snaptime", _/binary>>, _, State) ->
    {ok, State};

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          _ZoneSize:32/integer, _Zone:_ZoneSize/binary,
          _SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          _ModuleSize:32/integer, _Module:_ModuleSize/binary,
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          8:32/integer, "zonename", _/binary>>, _, State) ->
    {ok, State};

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          _ZoneSize:32/integer, _Zone:_ZoneSize/binary,
          _SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          _ModuleSize:32/integer, _Module:_ModuleSize/binary,
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          5:32/integer, "class", _/binary>>, _, State) ->
    {ok, State};

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
    tachyon_mps:provide(),
    {ok, State};

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          _ZoneSize:32/integer, Zone:_ZoneSize/binary,
          SnapTime:64/integer,
          _NameSize:32/integer, Name:_NameSize/binary,
          4:32/integer, "caps",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    case Name of
        %% CPU
        <<"cpucaps_zone_", _/binary>> ->
            put(<<"cloud.zones.cpu.", Key/binary>>, V, SnapTime,
                [{<<"zone">>, Zone}], State);
        %% Memory
        <<"physicalmem_zone_", _/binary>> ->
            put(<<"cloud.zones.mem.", Key/binary>>, V, SnapTime,
                [{<<"zone">>, Zone}], State);
        %% Swap
        <<"swapresv_zone_", _/binary>> ->
            put(<<"cloud.zones.swap.", Key/binary>>, V, SnapTime,
                [{<<"zone">>, Zone}], State);
        %% Procs
        <<"nprocs_zone_", _/binary>> ->
            put(<<"cloud.zones.swap.", Key/binary>>, V, SnapTime,
                [{<<"zone">>, Zone}], State);
        _ ->
            {ok,State}
    end;

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          _ZoneSize:32/integer, Zone:_ZoneSize/binary,
          SnapTime:64/integer,
          _NameSize:32/integer, IFInstance:_NameSize/binary,
          4:32/integer, "link",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    IFace = parse_iface(IFInstance),
    put(<<"cloud.zones.net.", Key/binary>>, V, SnapTime,
        [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State);

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          _ZoneSize:32/integer, Zone:_ZoneSize/binary,
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          8:32/integer, "zone_vfs",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    put(<<"cloud.zones.vfs.", Key/binary>>, V, SnapTime,
        [{<<"zone">>, Zone}], State);

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          _ZoneSize:32/integer, Zone:_ZoneSize/binary,
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          8:32/integer, "zone_zfs",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    put(<<"cloud.zones.zfs.", Key/binary>>, V, SnapTime,
        [{<<"zone">>, Zone}], State);

message(_Msg, _, State) ->
    tachyon_mps:provide(),
    {ok, State}.

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
    tachyon_mps:provide(),
    tachyon_mps:handle(),

    DBs1 = [{Mod, Mod:put(Metric, Value, Time, Args, DB)} ||
               {Mod, DB} <- DBs],
    {ok, State#state{db = DBs1}}.
