-module(tachyon_kstat_nsq).

-behaviour(ensq_channel_behaviour).

-export([response/1, message/2, error/1]).

response(_Msg) ->
    ok.

error(_Msg) ->
    ok.

message(Msg, _) ->
    case tachyon_kstat_pkg:decode(Msg) of
        {ok, P} ->
            tachyon_mps:inc(),
            tachyon_kstat:msg(P);
        E ->
            lager:error("[msg] ~p", [E])
    end.
