-module(tachyon_jmetric_nsq).

-behaviour(ensq_channel_behaviour).

-export([init/0, new_frame/1, response/2, message/3, error/2]).

init() ->
    {ok, []}.

new_frame(_State) ->
    {ok, []}.

response(_Msg, State) ->
    {ok, State}.

error(_Msg, State) ->
    {ok, State}.

%% message should look like
%% {
%%  "grouping": "grouping of the metric",
%%  "metric": "a.b.c",
%%  "time": 1234,
%%  "value", 1,
%%  "tags": [{"key":"value"}]
%% }

message(Msg, _, State) ->
    P = jsx:decode(Msg),
    G = proplists:get_value(<<"grouping">>, P),
    tachyon_mps:provide(),
    tachyon_metric:msg(G, P),
    State.
