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
-export([start_link/1, msg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(DB_SERVER, "127.0.0.1").
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
    {ok, DB} = {ok, 1}, % gen_tcp:connect(?DB_SERVER, ?DB_PORT, [{packet, line}]),
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

handle_cast({Host, <<"global">>, SnapTimem,
             {<<"sd">>, Instance, _Name, _Class}, {Key, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", Key/binary>>,
    tachyon_guard:put(Host, SnapTimem, Metric, V, 4),
    {noreply, State};

handle_cast({Host, <<"global">>, SnapTimem,
             {<<"sderr">>, Instance, _Name, _Class}, {<<"Hard Errors">>, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "hard_errors">>,
    tachyon_guard:put(Host, SnapTimem, Metric, V, 1),
    {noreply, State};

handle_cast({Host, <<"global">>, SnapTimem,
             {<<"sderr">>, Instance, _Name, _Class}, {<<"Soft Errors">>, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "soft_errors">>,
    tachyon_guard:put(Host, SnapTimem, Metric, V, 1),
    {noreply, State};

handle_cast({Host, <<"global">>, SnapTimem,
             {<<"sderr">>, Instance, _Name, _Class}, {<<"Transport Errors">>, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "transport_errors">>,
    tachyon_guard:put(Host, SnapTimem, Metric, V, 1),
    {noreply, State};

handle_cast({Host, <<"global">>, SnapTimem,
             {<<"sderr">>, Instance, _Name, _Class}, {<<"Illegal Request">>, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "illegal_requests">>,
    tachyon_guard:put(Host, SnapTimem, Metric, V, 1),
    {noreply, State};

handle_cast({Host, <<"global">>, SnapTimem,
             {<<"sderr">>, Instance, _Name, _Class}, {<<"Predictive Failure Analysis">>, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    Metric = <<"sd[", ID/binary, "].", "predicted_failures">>,
    tachyon_guard:put(Host, SnapTimem, Metric, V, 1),
    {noreply, State};

handle_cast({Host, <<"global">>, SnapTimem,
             {<<"cpu_stat">>, Instance, _Name, _Class}, {Key, V}},
            State) ->
    ID = list_to_binary(integer_to_list(Instance)),
    tachyon_guard:put(Host, SnapTimem, <<"cpu[", ID/binary, "].", Key/binary>>, V, 4),
    {noreply, State};

handle_cast({Host, Zone, SnapTimem, {Module, Instance, Name, Class}, {Key, V}},
            State = #state{
                       db = _DB,
                       host = Host}) ->
    io:format("[~s:~s@~p] ~s:~p:~s(~s) ~s = ~p~n",
              [Host, Zone, SnapTimem, Module, Instance, Class, Name, Key, V]),
    %% Median = case [C#packet_cpu.usage || C <- CPUs] of
    %%           [] ->
    %%               0;
    %%           CPUs1 ->
    %%               lists:nth(round(length(CPUs1)/2), lists:sort(CPUs1))
    %%       end,
    %% [Mem | _] = Packet#packet.mem,
    %% case {Host, Zone} of
    %%     {"headnode","global"} ->
    %%         lager:info("Processes: ~p", [Packet#packet.processes]);
    %%     _ ->
    %%         ok
    %% end,
    %% tachyon_guard:put(IP, Host, Time, "cpu.usage.median", Median/1000.0, 4),
    %% tachyon_guard:put(IP, Host, Time, "threads", Packet#packet.threads, 4),
    %% tachyon_guard:put(IP, Host, Time, "processes", Packet#packet.processes, 4),
    %% tachyon_guard:put(IP, Host, Time, "rss", Mem#packet_mem.rss, 4),
    %% tachyon_guard:put(IP, Host, Time, "swap", Mem#packet_mem.swap, 4),

    %% Metrics = fmt_packet(Packet),
    %% DB1 = case gen_tcp:send(DB, Metrics) of
    %%           {error, Reason} ->
    %%               timer:sleep(1000),
    %%               io:format("[~s] Socket died with: ~p", [IP, Reason]),
    %%               {ok, NewDB} = gen_tcp:connect(?DB_SERVER, ?DB_PORT, [{packet, line}]),
    %%               NewDB;
    %%           _ ->
    %%               DB
    %%       end,
    %% poll(IP),
    %%{noreply, State#state{db = DB1}};
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

%% fmt(S, P) -> io_lib:format(S, P).

%% fmt_packet(Packet) ->
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
