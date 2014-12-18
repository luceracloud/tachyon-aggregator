%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_mps).

-behaviour(gen_server).

%% API
-export([start_link/0, provide/0, handle/0, send/0]).
-ignore_xref([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(COUNTERS_PROV, tachyon_counters_provided).
-define(COUNTERS_HAND, tachyon_counters_handled).
-define(COUNTERS_SEND, tachyon_counters_send).
-record(state, {connection, node}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

provide() ->
    try
        ets:update_counter(?COUNTERS_PROV, self(), 1)
    catch
        error:badarg ->

            ets:insert(?COUNTERS_PROV, {self(), 1})
    end,
    ok.

handle() ->
    try
        ets:update_counter(?COUNTERS_HAND, self(), 1)
    catch
        error:badarg ->

            ets:insert(?COUNTERS_HAND, {self(), 1})
    end,
    ok.

send() ->
    try
        ets:update_counter(?COUNTERS_SEND, self(), 1)
    catch
        error:badarg ->

            ets:insert(?COUNTERS_SEND, {self(), 1})
    end,
    ok.



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
    erlang:send_after(1000, self(), tick),
    ets:new(?COUNTERS_PROV, [named_table, set, public, {write_concurrency, true}]),
    ets:new(?COUNTERS_HAND, [named_table, set, public, {write_concurrency, true}]),
    ets:new(?COUNTERS_SEND, [named_table, set, public, {write_concurrency, true}]),
    {ok, {Host, Port}} = application:get_env(tachyon, ddb_ip),
    Con = ddb_tcp:stream_mode(<<"tachyon">>, 2, ddb_tcp:connect(Host, Port)),
    Node = list_to_binary(atom_to_list(node())),
    {ok, #state{connection = Con, node = Node}}.

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
handle_info(tick, State = #state{connection = Con, node = Node}) ->
    {MegaSecs, Secs, _} = now(),
    T = (MegaSecs*1000000 + Secs),

    TblP = ets:tab2list(?COUNTERS_PROV),
    ets:delete_all_objects(?COUNTERS_PROV),
    P = lists:sum([N || {_, N} <- TblP]),

    TblS = ets:tab2list(?COUNTERS_SEND),
    ets:delete_all_objects(?COUNTERS_SEND),
    S = lists:sum([N || {_, N} <- TblS]),

    TblH = ets:tab2list(?COUNTERS_HAND),
    ets:delete_all_objects(?COUNTERS_HAND),
    H = lists:sum([N || {_, N} <- TblH]),

    Con1 = ddb_tcp:send([Node, <<"messages">>, <<"handled">>], H, T, Con),
    %% We send 3 metrics here so provided is + 3
    Con2 = ddb_tcp:send([Node, <<"messages">>, <<"provided">>], P + 3, T, Con1),
    Con3 = ddb_tcp:send([Node, <<"messages">>, <<"send">>], S, T, Con2),
    erlang:send_after(1000, self(), tick),
    {noreply, State#state{connection = Con3}};

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
