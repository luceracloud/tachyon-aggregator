-module(tachyon_kstat_nsq).

-behaviour(ensq_channel_behaviour).

-export([response/1, message/2, error/1]).

response(_Msg) ->
    ok.

error(_Msg) ->
    ok.

message(Msg, _) ->
    tachyon_mps:provide(),
    case tachyon_kstat_pkg:decode(Msg) of
        {ok, {Host, <<"global">>, _, _, _} = P} ->
            tproc:where({tachyon_kstat_host, Host}) ! P;
        {ok, {_, Zone, _, _, _} = P} ->
            tproc:where({tachyon_kstat_zone, Zone}) ! P;
        E ->
            lager:error("[msg] ~p", [E])
    end,
    ok.
