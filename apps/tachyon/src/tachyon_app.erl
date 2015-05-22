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
-export([make_rules/1, start/2, stop/1]).

-ignore_xref([make_rules/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

make_rules(File) ->
    {ok, Rules} = file:consult(File),
    ErlCode = tachyon_c:c(Rules),
    ErlFile = filename:join(code:priv_dir(tachyon), "tachyon_kstat.erl"),
    file:write_file(ErlFile, ErlCode),
    compile:file(ErlFile).

start(_StartType, _StartArgs) ->
    {ok,tachyon_kstat} = make_rules("etc/tachyon.rules"),
    tachyon_sup:start_link().

stop(_State) ->
    ok.
