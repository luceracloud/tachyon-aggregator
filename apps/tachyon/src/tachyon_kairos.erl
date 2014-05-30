%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_kairos).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([connect/0, put/5]).
-ignore_xref([put/5]).

-record(kairosdb, {enabled = true, db, host, port, cnt=0, max=100, acc=[]}).

connect() ->
    Max = case application:get_env(tachyon, kairosdb_buffer_size) of
              {ok, V} ->
                  V;
              _ ->
                  100
          end,
    case application:get_env(tachyon, kairosdb) of
        {ok, {Host, Port}} ->
            case gen_tcp:connect(Host, Port, []) of
                {ok, DB} ->
                    {ok, #kairosdb{enabled = true, db=DB, host=Host, port=Port,
                                  max = Max}};
                E ->
                    E
            end;
        _ ->
            #kairosdb{enabled = false}
    end.

put(Metric, Value, Time, Args,
    K = #kairosdb{enabled=true, db=DB, host=Host, port=Port, cnt=C, max=M,
                  acc=Acc}) when C > M ->
    Metrics = [fmt(Metric, Value, Time, Args) | Acc],
    K1 = case gen_tcp:send(DB, Metrics) of
             {error, Reason} ->
                 gen_tcp:close(DB),
                 lager:error("[~s] Socket died with: ~p", [Host, Reason]),
                 {ok, NewDB} = gen_tcp:connect(Host, Port, [], 100),
                 K#kairosdb{db=NewDB};
             _ ->
                 K
         end,
    tachyon_mps:send(),
    K1#kairosdb{acc = [], cnt = 0};

put(Metric, Value, Time, Args, K = #kairosdb{enabled = true, cnt=C, acc=Acc}) ->
    tachyon_mps:send(),
    K#kairosdb{cnt=C+1, acc=[fmt(Metric, Value, Time, Args) | Acc]};

put(_Metric, _Value, _Time, _Args, K) ->
    K.

fmt(Metric, Value, Time, Args) when is_integer(Value) ->
    [<<"put ", Metric/binary, $\s>>, integer_to_list(Time), $\s,
     integer_to_list(Value) | fmt_args(Args, <<"\n">>)];

fmt(Metric, Value, Time, Args) when is_float(Value) ->
    [<<"put ", Metric/binary, $\s>>, integer_to_list(Time), $\s,
     float_to_list(Value) | fmt_args(Args, <<"\n">>)].

fmt_args([{K, V}|R], Acc) when is_integer(V) ->
    fmt_args(R, [<<$\s, K/binary, $=>>, integer_to_list(V) | Acc]);

fmt_args([{K, V}|R], Acc) when is_float(V) ->
    fmt_args(R, [<<$\s, K/binary, $=>>, float_to_list(V) | Acc]);

fmt_args([{K, V}|R], Acc) when is_binary(V) orelse
                               is_list(V) ->
    fmt_args(R, [<<$\s, K/binary, $=>>, V | Acc]);

fmt_args([], Acc) ->
    Acc.

-ifdef(TEST).

fmt_test() ->
    Str = fmt(<<"a.metric">>, 0, 1, [{<<"hypervisor">>, <<"bla">>}, {<<"id">>, 1}]),
    Bin = list_to_binary(Str),
    ?assertEqual(<<"put a.metric 1 0 id=1 hypervisor=bla\n">>, Bin).

fmtno_arg_test() ->
    Str = fmt(<<"a.metric">>, 0, 1, []),
    Bin = list_to_binary(Str),
    ?assertEqual(<<"put a.metric 1 0\n">>, Bin).

-endif.
