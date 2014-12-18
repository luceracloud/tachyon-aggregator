%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 26 Jul 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon).

-export([start/0]).

-ignore_xref([start/0]).

start() ->
    application:start(sasl),
    application:start(inets),
    application:start(syntax_tools),
    application:start(compiler),
    application:start(goldrush),
    application:start(lager),
    application:start(ensq),
    application:start(tachyon).
