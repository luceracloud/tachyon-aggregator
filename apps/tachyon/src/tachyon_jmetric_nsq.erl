-module(tachyon_jmetric_nsq).

-behaviour(ensq_channel_behaviour).

-export([response/1, message/2, error/1]).

response(_Msg) ->
    ok.

error(_Msg) ->
    ok.


%% message should look like
%% {
%%  "grouping": "grouping of the metric",
%%  "metric": "a.b.c",
%%  "time": 1234,
%%  "value", 1,
%%  "tags": [{"key":"value"}]
%% }

message(Msg, _) ->
    P = jsx:decode(Msg),
    G = proplists:get_value(<<"grouping">>, P),
    tachyon_metric:msg(G, P).
