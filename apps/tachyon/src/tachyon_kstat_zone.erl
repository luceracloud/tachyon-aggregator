%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_kstat_zone).

%% API
-export([init/3, start/1]).
-ignore_xref([start/1]).

-record(state, {host, db}).

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
    Ref = make_ref(),
    spawn(tachyon_kstat_zone, init, [self(), Ref, Host]),
    receive
        {Ref, Reply} ->
            Reply
    after 5000 ->
            {error, timeout}
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
init(From, Ref, Host) ->
    process_flag(trap_exit, true),
    %% Setting the priority to hight ensure we go thourgh the data before we
    %% consume new one
    process_flag(priority, high),
    {ok, DB} = tachyon_kairos:connect(),
    {ok, Statsd} = tachyon_statsd:connect(),
    From ! {Ref, {ok, self()}},
    loop(#state{host = Host, db=[{tachyon_kairos, DB}, {tachyon_statsd, Statsd}]}).

loop(State) ->
    receive
        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"above_base_sec">>, V}} ->
            loop(put(<<"cloud.zones.cpu.above_base">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"above_sec">>, V}} ->
            loop(put(<<"cloud.zones.cpu.above">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"baseline">>, V}} ->
            loop(put(<<"cloud.zones.cpu.baseline">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"burst_limit_sec">>, V}} ->
            loop(put(<<"cloud.zones.cpu.burst_limit">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"burst_sec">>, V}} ->
            loop(put(<<"cloud.zones.cpu.burst">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"effective">>, V}} ->
            loop(put(<<"cloud.zones.cpu.effective">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"maxusage">>, V}} ->
            loop(put(<<"cloud.zones.cpu.maxusage">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"nwait">>, V}} ->
            loop(put(<<"cloud.zones.cpu.nwait">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"usage">>, V}} ->
            loop(put(<<"cloud.zones.cpu.usage">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
         {<<"value">>, V}} ->
            loop(put(<<"cloud.zones.cpu.value">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        %% Memory
        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"physicalmem_zone_", _/binary>>, _},
         {<<"usage">>, V}} ->
            loop(put(<<"cloud.zones.mem.usage">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"physicalmem_zone_", _/binary>>, _},
         {<<"value">>, V}} ->
            loop(put(<<"cloud.zones.mem.value">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        %% Swap
        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"swapresv_zone_", _/binary>>, _},
         {<<"usage">>, V}} ->
            loop(put(<<"cloud.zones.swap.usage">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"swapresv_zone_", _/binary>>, _},
         {<<"value">>, V}} ->
            loop(put(<<"cloud.zones.swap.value">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        %% Procs
        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"nprocs_zone_", _/binary>>, _},
         {<<"usage">>, V}} ->
            loop(put(<<"cloud.zones.procs.usage">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"caps">>, _, <<"nprocs_zone_", _/binary>>, _},
         {<<"value">>, V}} ->
            loop(put(<<"cloud.zones.procs.value">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        %% Net
        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"brdcstrcv">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.brdcstrcv">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"brdcstxmt">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.brdcstxmt">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"collisions">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.collisions">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"ierrors">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.ierrors">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"ipackets64">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.ipackets">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"multircv">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.multircv">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"multixmt">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.multixmt">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"norcvbuf">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.norcvbuf">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"noxmtbuf">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.noxmtbuf">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"obytes64">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.obytes">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"oerrors">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.oerrors">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"opackets64">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.opackets">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        {_Host, Zone, SnapTime,
         {<<"link">>, _, IFInstance, _},
         {<<"rbytes64">>, V}} ->
            IFace = parse_iface(IFInstance),
            loop(put(<<"cloud.zones.net.rbytes">>, V, SnapTime,
                     [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State));

        %% VFS
        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"100ms_ops">>, V}} ->
            loop(put(<<"cloud.zones.vfs.ops.100ms">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"10ms_ops">>, V}} ->
            loop(put(<<"cloud.zones.vfs.ops.10ms">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"10s_ops">>, V}} ->
            loop(put(<<"cloud.zones.vfs.ops.10s">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"1s_ops">>, V}} ->
            loop(put(<<"cloud.zones.vfs.ops.1s">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"delay_cnt">>, V}} ->
            loop(put(<<"cloud.zones.vfs.delay_cnt">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"delay_time">>, V}} ->
            loop(put(<<"cloud.zones.vfs.delay_time">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"nread">>, V}} ->
            loop(put(<<"cloud.zones.vfs.read.n">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"reads">>, V}} ->
            loop(put(<<"cloud.zones.vfs.read.count">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"rlentime">>, V}} ->
            loop(put(<<"cloud.zones.vfs.read.lentime">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"rtime">>, V}} ->
            loop(put(<<"cloud.zones.vfs.read.time">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"nwritten">>, V}} ->
            loop(put(<<"cloud.zones.vfs.write.n">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"writes">>, V}} ->
            loop(put(<<"cloud.zones.vfs.write.count">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));


        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"wlentime">>, V}} ->
            loop(put(<<"cloud.zones.vfs.write.lentime">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_vfs">>, _, _, _},
         {<<"wtime">>, V}} ->
            loop(put(<<"cloud.zones.vfs.write.time">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));


        %% ZFS
        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"nread">>, V}} ->
            loop(put(<<"cloud.zones.zfs.read.n">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"reads">>, V}} ->
            loop(put(<<"cloud.zones.zfs.read.count">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));


        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"rlentime">>, V}} ->
            loop(put(<<"cloud.zones.zfs.read.lentime">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"rtime">>, V}} ->
            loop(put(<<"cloud.zones.zfs.read.time">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"nwritten">>, V}} ->
            loop(put(<<"cloud.zones.zfs.write.n">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"writes">>, V}} ->
            loop(put(<<"cloud.zones.zfs.write.count">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"wlentime">>, V}} ->
            loop(put(<<"cloud.zones.zfs.write.lentime">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));


        {_Host, Zone, SnapTime,
         {<<"zone_zfs">>, _, _, _},
         {<<"wtime">>, V}} ->
            loop(put(<<"cloud.zones.zfs.write.time">>, V, SnapTime,
                     [{<<"zone">>, Zone}], State));

        {'EXIT', _, _} ->
            ok;

        _ ->
            loop(State)
    end.

%% {Host, Zone, SnapTime, {Module, Instance, Name, Class}, {Key, V}} ->
%%     lager:debug("[~s:~s@~p] "
%%                 "~s:~p:~s(~s) "
%%                 "~s = ~p~n",
%%                 [Host, Zone, SnapTime,
%%                  Module, Instance, Class, Name,
%%                  Key, V]),
%%     {noreply, State}.


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
    State#state{db = DBs1}.
