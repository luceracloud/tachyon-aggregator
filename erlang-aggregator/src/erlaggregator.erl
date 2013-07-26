%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(erlaggregator).

-export([start/0, stats/0, add/1, remove/1]).

start() ->
    application:start(erlzmq),
    application:start(erlaggregator).

stats() ->
    erlaggregator_server:stats().

add(IP) ->
    erlaggregator_probe_sup:start_child(IP).

remove(IP) ->
    erlaggregator_probe:stop(IP).
