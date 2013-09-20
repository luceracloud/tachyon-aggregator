%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(erlaggregator_probe).

-behaviour(gen_server).
-include("packet_pb.hrl").

%% API
-export([start_link/1, poll/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(DB_SERVER, "127.0.0.1").
-define(DB_PORT, 4242).

-record(state, {tcp, sock, ip, db}).

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
start_link(IP) ->
    gen_server:start_link({local, list_to_atom(IP)}, ?MODULE, [IP], []).

poll(IP) ->
    gen_server:cast(list_to_atom(IP), poll).

stop(IP) ->
    gen_server:cast(list_to_atom(IP), stop).
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
init([IP]) ->
    process_flag(trap_exit, true),
    {ok, Ctx} = erlaggregator_server:ctx(),
    {ok, Sock} = erlzmq:socket(Ctx, pull),
    erlzmq:connect(Sock, "tcp://" ++ IP ++ ":7211"),
    {ok, DB} = gen_tcp:connect(?DB_SERVER, ?DB_PORT, [{packet, line}]),
    poll(IP),
    {ok, #state{sock = Sock, ip = IP, db=DB}}.

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
handle_cast(poll, State =
                #state{sock = Sock,
                       db = DB,
                       ip = IP}) ->
    {ok, Data} = erlzmq:recv(Sock),
    Packet = packet_pb:decode_packet(Data),
    Host = Packet#packet.name,
    Time = Packet#packet.time,
    send(DB, Time, Host, "machines.threads", Packet#packet.threads),
    send(DB, Time, Host, "machines.processes", Packet#packet.processes),
    [gen_tcp:send(DB,
       io_lib:format("put machines.cpu ~p ~p host=~s cpu=~p~n",
                     [Time, CPU#packet_cpu.usage, Host, CPU#packet_cpu.core]))
       || CPU <- Packet#packet.cpu],
    [Mem | _] = Packet#packet.mem,
    send(DB, Time, Host, "machines.rss", Mem#packet_mem.rss), 
    send(DB, Time, Host, "machines.swap", Mem#packet_mem.swap), 
    CPUData = [
       io_lib:format("put machines.cpu ~p ~p host=~s cpu=~p~n",
                     [Time, CPU#packet_cpu.usage, Host, CPU#packet_cpu.core])
       || CPU <- Packet#packet.cpu],
    gen_tcp:send(DB, CPUData),
    NetData = [fmt_net(Time, Host, Net) || Net <- Packet#packet.net],
    gen_tcp:send(DB, NetData),
    DiskData = [fmt_disk(Time, Host, Disk) || Disk <- Packet#packet.disk],
    gen_tcp:send(DB, DiskData),
    poll(IP),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
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
terminate(_Reason, State) ->
    erlzmq:close(State#state.sock, 5000),
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
send(DB, Time, Name, Metric, Value) ->
    gen_tcp:send(DB,
      io_lib:format("put ~s ~p ~p host=~s~n",
                    [Metric, Time, Value, Name])).

fmt_disk(Time, Name, P = #packet_disk{
  instance = Instance
}) ->
  [io_lib:format("put machines.disk.~s ~p ~p host=~s interface=~s~n",
                  [Metric, Time, def(Value), Name, Instance]) ||
   {Metric, Value} <- [
      {"nread", P#packet_disk.nread},
      {"nwritten", P#packet_disk.nwritten},
      {"reads", P#packet_disk.reads},
      {"writes", P#packet_disk.writes},
      {"rtime", P#packet_disk.rtime},
      {"wtime", P#packet_disk.wtime},
      {"rlentime", P#packet_disk.rlentime},
      {"wlentime", P#packet_disk.wlentime},
      {"harderror", P#packet_disk.harderror},
      {"softerror", P#packet_disk.softerror},
      {"tranerror", P#packet_disk.tranerror}]].

def(undefined) -> 0;
def(V) -> V.

fmt_net(Time, Name, #packet_net{
    obytes64 = OBytes,
    rbytes64 = RBytes,
    opackets = OPkgs,
    ipackets = IPkgs,
    oerrors = OErrors,
    ierrors = IErrors,
    instance = I0
  }) ->
  IFace = case I0 of
    undefined ->
      "vm-local";
    _ ->
      I0
  end,
  [io_lib:format("put machines.net.~s ~p ~p host=~s interface=~s~n",
                  [Metric, Time, Value, Name, IFace]) ||
   {Metric, Value} <- [
      {"obytes", OBytes},
      {"rbytes", RBytes},
      {"opkgs", OPkgs},
      {"ipkgs", IPkgs},
      {"oerrors", OErrors},
      {"ierrors", IErrors}]].
