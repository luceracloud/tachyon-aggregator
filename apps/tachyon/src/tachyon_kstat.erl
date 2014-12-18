%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_kstat).

-behaviour(ensq_channel_behaviour).

-record(state, {server, zone}).

-export([init/0, response/2, message/3, error/2]).


init() ->
    {ok, {Host, Port}} = application:get_env(tachyon, ddb_ip),
    {ok, Srvs} = ddb_tcp:connect(Host, Port),
    {ok, Srvs1} = ddb_tcp:stream_mode(<<"server">>, 2, Srvs),
    {ok, Zones} = ddb_tcp:connect(Host, Port),
    {ok, Zones1} = ddb_tcp:stream_mode(<<"zone">>, 2, Zones),
    {ok, #state{server = Srvs1, zone = Zones1}}.

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
    puts([Host, <<"ip">>, Key], V, SnapTime, State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          8:32/integer, "arcstats",
          3:32/integer, "zfs",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    puts([Host, <<"arc">>, Key], V, SnapTime, State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          2:32/integer, "sd",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    puts([Host, <<"disk">>, integer_to_binary(Instance), <<"metrics">>, Key],
         V, SnapTime, State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          5:32/integer, "sderr",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          11:32/integer, "Hard Errors",
          $i, V:64/integer>>, _, State) ->
    puts([Host, <<"disk">>, integer_to_binary(Instance), <<"errors">>,
          <<"hard">>], V, SnapTime,
         State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          5:32/integer, "sderr",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          11:32/integer, "Soft Errors",
          $i, V:64/integer>>, _, State) ->
    puts([Host, <<"disk">>, integer_to_binary(Instance), <<"errors">>,
          <<"soft">>], V, SnapTime,
         State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          5:32/integer, "sderr",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          16:32/integer, "Transport Errors",
          $i, V:64/integer>>, _, State) ->
    puts([Host, <<"disk">>, Instance, <<"errors">>, <<"transport">>], V,
         SnapTime, State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          5:32/integer, "sderr",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          27:32/integer, "Predictive Failure Analysis",
          $i, V:64/integer>>, _, State) ->
    puts([Host, <<"disk">>, integer_to_binary(Instance), <<"errors">>,
          <<"predicted_failures">>],
         V, SnapTime, State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          5:32/integer, "sderr",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          15:32/integer, "Illegal Request",
          $i, V:64/integer>>, _, State) ->
    puts([Host, <<"disk">>, integer_to_binary(Instance), <<"errors">>,
          <<"illegal">>], V, SnapTime,
         State);

%%
%% CPU Load
%%
message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          8:32/integer, "cpu_stat",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    puts([Host, <<"cpu">>, integer_to_binary(Instance), Key], V, SnapTime,
         State);

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
    puts([Host, <<"ip_nic_event_queue">>, Key], V, SnapTime, State);

message(<<_HostSize:32/integer, Host:_HostSize/binary,
          6:32/integer, "global",
          SnapTime:64/integer,
          _NameSize:32/integer, Name:_NameSize/binary,
          4:32/integer, "unix",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    puts([Host, <<"cache">>, Name, Key], V, SnapTime, State);

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
            putz([Zone, <<"cpu">>, Key], V, SnapTime,
            State);
        %% Memory
        <<"physicalmem_zone_", _/binary>> ->
            putz([Zone, <<"mem">>, Key], V, SnapTime, State);
        %% Swap
        <<"swapresv_zone_", _/binary>> ->
            putz([Zone, <<"swap">>, Key], V, SnapTime, State);
        %% Procs
        <<"nprocs_zone_", _/binary>> ->
            putz([Zone, <<"swap">>, Key], V, SnapTime, State);
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
    putz([Zone, <<"net">>, IFace, Key], V, SnapTime, State);

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          _ZoneSize:32/integer, Zone:_ZoneSize/binary,
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          8:32/integer, "zone_vfs",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    putz([Zone, <<"vfs">>, Key], V, SnapTime, State);

message(<<_HostSize:32/integer, _Host:_HostSize/binary,
          _ZoneSize:32/integer, Zone:_ZoneSize/binary,
          SnapTime:64/integer,
          _NameSize:32/integer, _Name:_NameSize/binary,
          8:32/integer, "zone_zfs",
          _ClassSize:32/integer, _Class:_ClassSize/binary,
          _Instance:64/integer,
          _KeySize:32/integer, Key:_KeySize/binary,
          $i, V:64/integer>>, _, State) ->
    putz([Zone, <<"zfs">>, Key], V, SnapTime, State);

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


puts(Metric, Value, Time, State = #state{server = Con}) ->
    tachyon_mps:send(),
    tachyon_mps:provide(),
    tachyon_mps:handle(),
    Metric2 = dproto:metric_from_list(Metric),
    Con1 = ddb_tcp:send(Metric2, Time, mmath_bin:from_list([Value]), Con),
    {ok, State#state{server = Con1}}.

putz(Metric, Value, Time, State = #state{zone = Con}) ->
    tachyon_mps:send(),
    tachyon_mps:provide(),
    tachyon_mps:handle(),
    Metric2 = dproto:metric_from_list(Metric),
    Con1 = ddb_tcp:send(Metric2, Time, mmath_bin:from_list([Value]), Con),
    {ok, State#state{zone = Con1}}.
