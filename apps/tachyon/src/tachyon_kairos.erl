-module(tachyon_kairos).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([connect/0, put/5]).
-record(kairosdb, {enabled = true, db, host, port}).

connect() ->
    case application:get_env(tachyon, kairosdb) of
        {ok, {Host, Port}} ->
            case gen_tcp:connect(Host, Port, [{packet, line}]) of
                {ok, DB} ->
                    {ok, #kairosdb{enabled = true, db=DB, host=Host, port=Port}};
                E ->
                    E
            end;
        _ ->
            #kairosdb{enabled = false}
    end.

put(Metric, Value, Time, Args, K = #kairosdb{enabled = true, db=DB, host=Host, port=Port}) ->
    Metrics = fmt(Metric, Value, Time, Args),
    DB1 = case gen_tcp:send(DB, Metrics) of
              {error, Reason} ->
                  gen_tcp:close(DB),
                  lager:error("[~s] Socket died with: ~p", [Host, Reason]),
                  {ok, NewDB} = gen_tcp:connect(Host, Port, [{packet, line}], 100),
                  K#kairosdb{db=NewDB};
              _ ->
                  K
          end,
    DB1;

put(_Metric, _Value, _Time, _Args, DB) ->
    DB.

fmt(Metric, Value, Time, Args) ->
    ["put ", Metric, $\s, integer_to_list(Time), $ , integer_to_list(Value) |
     fmt_args(Args, "\n")].

fmt_args([{K, V}|R], Acc) when is_atom(K) ->
    fmt_args([{atom_to_list(K), V}|R], Acc);
fmt_args([{K, V}|R], Acc) when is_integer(V) ->
    fmt_args(R, [$\s, K, $=, integer_to_list(V) | Acc]);
%% watch the      ^- space here
fmt_args([{K, V}|R], Acc) when is_float(V) ->
    fmt_args(R, [$\s, K, $=, float_to_list(V) | Acc]);
%% watch the      ^- space here
fmt_args([{K, V}|R], Acc) when is_binary(V) orelse
                               is_list(V) ->
    fmt_args(R, [$\s, K, $=, V | Acc]);
%% watch the      ^- space here

fmt_args([], Acc) ->
    Acc.

-ifdef(TEST).

fmt_test() ->
    Str = fmt(<<"a.metric">>, 0, 1, [{hypervisor, <<"bla">>}, {id, 1}]),
    Bin = list_to_binary(Str),
    ?assertEqual(<<"put a.metric 1 0 id=1 hypervisor=bla\n">>, Bin).

fmtno_arg_test() ->
    Str = fmt(<<"a.metric">>, 0, 1, []),
    Bin = list_to_binary(Str),
    ?assertEqual(<<"put a.metric 1 0\n">>, Bin).

-endif.
