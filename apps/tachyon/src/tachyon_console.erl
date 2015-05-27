-module(tachyon_console).

-export([load_rules/1, test_rules/1]).

-ignore_xref([test_rules/1]).

test_rules(File) ->
    load_rules(tachyon_kstat_test, File).

load_rules(File) ->
    load_rules(tachyon_kstat, File).

load_rules(Module, File) ->
    ModuleS = atom_to_list(Module),
    ErlFile = "/tmp/" ++ ModuleS ++ ".erl",
    BeamFile = ModuleS ++ ".beam",
    Opts = [verbose, report_errors, report_warnings, binary],
    p("Compilation started."),
    p("Rule File:           ~s", [File]),
    p("Output File:         ~s", [ErlFile]),
    p("Target Module:       ~s", [Module]),
    p("BEAM Filename:       ~s", [BeamFile]),
    p("Compilation Options: ~p", [Opts]),
    p(""),
    case tachyon_c:c(Module, File) of
        {ok, ErlCode} ->
            p("Compilation succesful."),
            case file:write_file(ErlFile, ErlCode) of
                ok ->
                    p("Output written."),
                    case compile:file(ErlFile, Opts) of
                        {ok, Module, Bin} ->
                            p("Compiled to BEAM."),
                            case code:load_binary(Module, BeamFile, Bin) of
                                {module, Module} ->
                                    p("Rule loading successfull!"),
                                    ok;
                                E ->
                                    e("Filed to load beam file", E)
                            end;
                        E ->
                            e("Filed to compile to beam", E)
                    end;
                E ->
                    e("Failed to write output file", E)
            end;
        E ->
            e("Compilation failed with", E)
    end.


e(S, E) ->
    p(S ++ ": ~p", [E]),
    error.

p(S) ->
    p(S, []).

p(S, A) ->
    lager:info("[rules] " ++ S, A),
    io:format(S ++ "\n", A).
