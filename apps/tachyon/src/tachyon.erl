%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon).

-export([start/0, stats/0, stats/1, add/1, remove/1]).

-xref_ignore([start/0, stats/0, stats/1, add/1, remove/1]).

start() ->
    application:start(sasl),
    application:start(inets),
    application:start(syntax_tools),
    application:start(compiler),
    application:start(goldrush),
    application:start(lager),
    application:start(ensq),
    application:start(tachyon).

stats() ->
    tachyon_guard:stats().

stats(IP) ->
    tachyon_guard:stats(IP).

add(IP) ->
    tachyon_guard_sup:start_child(IP),
    tachyon_probe_sup:start_child(IP).

remove(IP) ->
    tachyon_probe:stop(IP).
