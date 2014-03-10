%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_probe).

-behaviour(gen_server).

%% API
-export([start_link/1, msg/1, fmt/4]).
-ignore_xref([start_link/1, fmt/4]).

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
start_link(Host) ->
    gen_server:start_link({global, {host, Host}}, ?MODULE, [Host], []).

msg({Host, _, _, _, _} = P) ->
    case global:whereis_name({host, Host}) of
        undefined ->
            {ok, Pid} = tachyon_probe_sup:start_child(Host),
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
init([Host]) ->
    process_flag(trap_exit, true),
    {ok, DB} = gen_tcp:connect(?DB_SERVER, ?DB_PORT, [{packet, line}]),
    {ok, #state{host = Host, db=DB}}.

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
%% put(Host, Time, Metric, Value, T)

%% Disks
handle_cast({Host, <<"global">>, SnapTime,
             {<<"sd">>, Instance, _Name, _Class}, {Key, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", Key/binary>>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 4),
    State1 = put(<<"cloud.host.disk.metrics.", Key/binary>>, V, SnapTime,
                 [{disk, Instance}, {host, Host}], State),
    {noreply, State1};

handle_cast({Host, <<"global">>, SnapTime,
             {<<"sderr">>, Instance, _Name, _Class}, {<<"Hard Errors">>, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].errors.hard">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    State1 = put(<<"cloud.host.disk.errors.hard">>, V, SnapTime,
                 [{disk, Instance}, {host, Host}], State),
    {noreply, State1};

handle_cast({Host, <<"global">>, SnapTime,
             {<<"sderr">>, Instance, _Name, _Class}, {<<"Soft Errors">>, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].errors.hard">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    State1 = put(<<"cloud.host.disk.errors.soft">>, V, SnapTime,
                 [{disk, Instance}, {host, Host}], State),
    {noreply, State1};

handle_cast({Host, <<"global">>, SnapTime,
             {<<"sderr">>, Instance, _Name, _Class}, {<<"Transport Errors">>, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].errors.transport">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    State1 = put(<<"cloud.host.disk.errors.transport">>, V, SnapTime,
                 [{disk, Instance}, {host, Host}], State),
    {noreply, State1};

handle_cast({Host, <<"global">>, SnapTime,
             {<<"sderr">>, Instance, _Name, _Class}, {<<"Illegal Request">>, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "illegal_requests">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    State1 = put(<<"cloud.host.disk.errors.illegal">>, V, SnapTime,
                 [{disk, Instance}, {host, Host}], State),
    {noreply, State1};

handle_cast({Host, <<"global">>, SnapTime,
             {<<"sderr">>, Instance, _Name, _Class}, {<<"Predictive Failure Analysis">>, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "predicted_failures">>,
    tachyon_guard:put(Host, SnapTime, Metric, V, 1),
    State1 = put(<<"cloud.host.disk.errors.predicted_failures">>, V, SnapTime,
                 [{disk, Instance}, {host, Host}], State),
    {noreply, State1};

%% CPU Load
handle_cast({Host, <<"global">>, SnapTime,
             {<<"cpu_stat">>, Instance, _Name, _Class}, {Key, V}},
            State) ->

    ID = list_to_binary(integer_to_list(Instance)),
    tachyon_guard:put(Host, SnapTime, <<"cpu[", ID/binary, "].", Key/binary>>, V, 4),
    State1 = put(<<"cloud.host.cpu.", Key/binary>>, V, SnapTime,
                 [{cpu, Instance}, {host, Host}], State),
    {noreply, State1};

handle_cast({Host, Zone, SnapTime, {Module, Instance, Name, Class}, {Key, V}},
            State = #state{
                       db = _DB,
                       host = Host}) ->
    lager:debug("[~s:~s@~p] ~s:~p:~s(~s) ~s = ~p~n",
                [Host, Zone, SnapTime, Module, Instance, Class, Name, Key, V]),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("Unknown message: ~p~n", [Msg]),
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

fmt(Metric, Value, Time, Args) ->
    {Fmt, Vals} =
        lists:foldl(fun({K, V}, {S, A}) when is_binary(V)->
                            {S ++ " ~p=~s", [V, K | A]};
                       ({K, V}, {S, A}) when is_number(V) ->
                            {S ++ " ~p=~p", [V, K | A]};
                       (_, Acc) ->
                            Acc
                    end, {"put ~s ~p ~p", [Value, Time, Metric]}, Args),
    io_lib:format(Fmt ++ "~n", lists:reverse(Vals)).

put(Metric, Value, Time, Args, State = #state{db = DB, host=Host}) ->
    Metrics = fmt(Metric, Value, Time, Args),
    DB1 = case gen_tcp:send(DB, Metrics) of
              {error, Reason} ->
                  lager:error("[~s] Socket died with: ~p", [Host, Reason]),
                  {ok, NewDB} = gen_tcp:connect(?DB_SERVER, ?DB_PORT, [{packet, line}]),
                  NewDB;
              _ ->
                  DB
          end,
    State#state{db = DB1}.


%% fmt(S, P) -> io_lib:format(S, P).

%% fmt_packet(Packet) ->d
%%     Zone = Packet#packet.zone,
%%     Host = Packet#packet.host,
%%     Time = Packet#packet.time,
%%     [Mem | _] = Packet#packet.mem,
%%     Base = fmt_generic(Time, Host, Zone, [{"cloud.threads", Packet#packet.threads},
%%                                           {"cloud.processes", Packet#packet.processes},
%%                                           {"cloud.rss", Mem#packet_mem.rss},
%%                                           {"cloud.swap", Mem#packet_mem.swap}]),
%%     CPU1Data = [
%%                 fmt("put cloud.cpu.usage ~p ~p host=~s zone=~s cpu=~p~n",
%%                     [Time, CPU#packet_cpu.usage / 10000, Host, Zone, CPU#packet_cpu.core])
%%                 || CPU <- Packet#packet.cpu],

%%     CPU2Data = [
%%                 fmt("put cloud.cpu.queue.length ~p ~p host=~s zone=~s cpu=~p~n",
%%                     [Time, CPU#packet_cpuqueue.length, Host, Zone, CPU#packet_cpuqueue.core])
%%                 || CPU <- Packet#packet.cpuqueue],
%%     CPU3Data = [
%%                 fmt("put cloud.cpu.queue.amount ~p ~p host=~s zone=~s cpu=~p~n",
%%                     [Time, CPU#packet_cpuqueue.amount, Host, Zone, CPU#packet_cpuqueue.core])
%%                 || CPU <- Packet#packet.cpuqueue],
%%     NetData = [fmt_net(Time, Host, Zone, Net) || Net <- Packet#packet.net],
%%     DiskData = [fmt_disk(Time, Host, Zone, Disk) || Disk <- Packet#packet.disk],
%%     ProcData = [fmt("put cloud.proc ~p ~p executable=~s host=~s zone=~s~n",
%%                     [Time, P#packet_process.usage, P#packet_process.execname, Host, Zone])
%%                 || P <- Packet#packet.process],
%%     CF1Data = [fmt("put cloud.callfreq.value ~p ~p host=~s zone=~s call=~s~n",
%%                    [Time, P#packet_callfreq.value, Host, Zone, P#packet_callfreq.name])
%%                || P <- Packet#packet.callfreq],
%%     CF2Data = [fmt("put cloud.callfreq.time ~p ~p host=~s zone=~s call=~s~n",
%%                    [Time, P#packet_callfreq.time, Host, Zone, P#packet_callfreq.name])
%%                || P <- Packet#packet.callfreq],
%%     [Base, DiskData, NetData, CPU1Data, CPU2Data, CPU3Data, ProcData, CF1Data, CF2Data].

%% fmt_generic(Time, Host, Zone, Metrics) ->
%%     [fmt_generic(Time, Host, Zone, Metric, Value) ||
%%         {Metric, Value} <- Metrics].

%% fmt_generic(Time, Host, Zone, Metric, Value) ->
%%     fmt("put ~s ~p ~p host=~s zone=~s~n",
%%         [Metric, Time, Value, Host, Zone]).

%% fmt_disk(Time, Host, Zone, P = #packet_disk{
%%                                   instance = Instance
%%                                  }) ->
%%     [fmt("put cloud.disk.~s ~p ~p host=~s zone=~s instance=~s~n",
%%          [Metric, Time, def(Value), Host, Zone, Instance]) ||
%%         {Metric, Value} <- [
%%                             {"nread", P#packet_disk.nread},
%%                             {"nwrittenc", P#packet_disk.nwritten},
%%                             {"reads", P#packet_disk.reads},
%%                             {"writes", P#packet_disk.writes},
%%                             {"rtime", P#packet_disk.rtime},
%%                             {"wtime", P#packet_disk.wtime},
%%                             {"rlentime", P#packet_disk.rlentime},
%%                             {"wlentime", P#packet_disk.wlentime},
%%                             {"harderror", P#packet_disk.harderror},
%%                             {"softerror", P#packet_disk.softerror},
%%                             {"tranerror", P#packet_disk.tranerror}]].

%% def(undefined) -> 0;
%% def(V) -> V.

%% fmt_net(Time, Host, Zone, #packet_net{
%%                              obytes64 = OBytes,
%%                              rbytes64 = RBytes,
%%                              opackets = OPkgs,
%%                              ipackets = IPkgs,
%%                              oerrors = OErrors,
%%                              ierrors = IErrors,
%%                              instance = I0
%%                             }) ->
%%     IFace = case I0 of
%%                 undefined ->
%%                     "vm-local";
%%                 _ ->
%%                     I0
%%             end,
%%     [fmt("put cloud.net.~s ~p ~p host=~s zone=~s interface=~s~n",
%%          [Metric, Time, Value, Host, Zone, IFace]) ||
%%         {Metric, Value} <- [
%%                             {"obytes", OBytes},
%%                             {"rbytes", RBytes},
%%                             {"opkgs", OPkgs},
%%                             {"ipkgs", IPkgs},
%%                             {"oerrors", OErrors},
%%                             {"ierrors", IErrors}]].
