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
    lager:info("[rules] Compileing the rule from file: ~s", [File]),
    ErlCode = tachyon_c:c(File),
    ErlFile = filename:join(code:priv_dir(tachyon), "tachyon_kstat.erl"),
    lager:info("[rules] Writing resulting erlang code to: ~s.", [ErlFile]),
    file:write_file(ErlFile, ErlCode),
    lager:info("[rules] Compiling Erlang code."),
    Opts = [verbose,report_errors,report_warnings, binary],
    {ok, tachyon_kstat, Bin} = compile:file(ErlFile, Opts),
    code:load_binary(tachyon_kstat, "tachyon_kstat.beam", Bin).

start(_StartType, _StartArgs) ->
    {module, tachyon_kstat} = make_rules("etc/tachyon.rules"),
    tachyon_sup:start_link().

stop(_State) ->
    ok.
