%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon).

-export([start/0, stats/0, add/1, remove/1]).

start() ->
    application:start(sasl),
    application:start(erlzmq),
    application:start(lager),
    application:start(tachyon).

stats() ->
    tachyon_server:stats().

add(IP) ->
    tachyon_guard_sup:start_child(IP),
    tachyon_probe_sup:start_child(IP).

remove(IP) ->
    tachyon_probe:stop(IP).
