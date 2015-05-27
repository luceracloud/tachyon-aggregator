%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ignore_xref([make_rules/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    ok = tachyon_console:load_rules("etc/tachyon.rules"),
    tachyon_sup:start_link().

stop(_State) ->
    ok.
