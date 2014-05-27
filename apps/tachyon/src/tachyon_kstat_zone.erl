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
start_link(Zone) ->
    gen_server:start_link({global, {zone, Zone}}, ?MODULE, [], []).

start(Zone) ->
    tachyon_kstat_zone_sup:start_child(Zone).

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
    {ok, DB} = tachyon_kairos:connect(),
    {ok, Statsd} = tachyon_statsd:connect(),
    {ok, #state{db=[{tachyon_kairos, DB}, {tachyon_statsd, Statsd}]}}.

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
    handle_zone(M, State);

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

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"above_base_sec">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.above_base">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"above_sec">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.above">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"baseline">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.baseline">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"burst_limit_sec">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.burst_limit">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"burst_sec">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.burst">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"effective">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.effective">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"maxusage">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.maxusage">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"nwait">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.nwait">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"usage">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.usage">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"cpucaps_zone_", _/binary>>, _},
             {<<"value">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.cpu.value">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

%% Memory

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"physicalmem_zone_", _/binary>>, _},
             {<<"usage">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.mem.usage">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"physicalmem_zone_", _/binary>>, _},
             {<<"value">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.mem.value">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

%% Swap

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"swapresv_zone_", _/binary>>, _},
             {<<"usage">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.swap.usage">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"swapresv_zone_", _/binary>>, _},
             {<<"value">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.swap.value">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

%% Procs

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"nprocs_zone_", _/binary>>, _},
             {<<"usage">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.procs.usage">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"caps">>, _, <<"nprocs_zone_", _/binary>>, _},
             {<<"value">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.procs.value">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};


%% Net

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"brdcstrcv">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.brdcstrcv">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"brdcstxmt">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.brdcstxmt">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"collisions">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.collisions">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"ierrors">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.ierrors">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"ipackets64">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.ipackets">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"multircv">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.multircv">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"multixmt">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.multixmt">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"norcvbuf">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.norcvbuf">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"noxmtbuf">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.noxmtbuf">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"obytes64">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.obytes">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"oerrors">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.oerrors">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"opackets64">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.opackets">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"link">>, _, IFInstance, _},
             {<<"rbytes64">>, V}},
            State) ->
    IFace = parse_iface(IFInstance),
    State1 = put(<<"cloud.zones.net.rbytes">>, V, SnapTime,
                 [{<<"zone">>, Zone}, {<<"interface">>, IFace}], State),
    {noreply, State1};

%% VFS

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"100ms_ops">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.ops.100ms">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"10ms_ops">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.ops.10ms">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"10s_ops">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.ops.10s">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"1s_ops">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.ops.1s">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"delay_cnt">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.delay_cnt">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"delay_time">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.delay_time">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"nread">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.read.n">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"reads">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.read.count">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"rlentime">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.read.lentime">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"rtime">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.read.time">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"nwritten">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.write.n">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};
handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"writes">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.write.count">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"wlentime">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.write.lentime">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_vfs">>, _, _, _},
             {<<"wtime">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.vfs.write.time">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};


%% ZFS

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_zfs">>, _, _, _},
             {<<"nread">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.zfs.read.n">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_zfs">>, _, _, _},
             {<<"reads">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.zfs.read.count">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_zfs">>, _, _, _},
             {<<"rlentime">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.zfs.read.lentime">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_zfs">>, _, _, _},
             {<<"rtime">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.zfs.read.time">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_zfs">>, _, _, _},
             {<<"nwritten">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.zfs.write.n">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};
handle_zone({_Host, Zone, SnapTime,
             {<<"zone_zfs">>, _, _, _},
             {<<"writes">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.zfs.write.count">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_zfs">>, _, _, _},
             {<<"wlentime">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.zfs.write.lentime">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

handle_zone({_Host, Zone, SnapTime,
             {<<"zone_zfs">>, _, _, _},
             {<<"wtime">>, V}},
            State) ->
    State1 = put(<<"cloud.zones.zfs.write.time">>, V, SnapTime,
                 [{<<"zone">>, Zone}], State),
    {noreply, State1};

%% handle_zone({Host, Zone, SnapTime, {Module, Instance, Name, Class}, {Key, V}}, State) ->
%%     lager:debug("[~s:~s@~p] "
%%                 "~s:~p:~s(~s) "
%%                 "~s = ~p~n",
%%                 [Host, Zone, SnapTime,
%%                  Module, Instance, Class, Name,
%%                  Key, V]),
%%     {noreply, State}.

handle_zone({_, _, _, {_, _, _, _}, {_, _}}, State) ->
    {noreply, State}.


parse_iface(<<$_, IFace/binary>>) ->
    IFace;
parse_iface(<<_, Rest/binary>>) ->
    parse_iface(Rest);
parse_iface(<<>>) ->
    <<"net">>.

