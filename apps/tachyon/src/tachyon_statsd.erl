%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_statsd).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([connect/0, put/5]).
-ignore_xref([put/5]).
-record(statsd, {enabled, db, host, port, cnt=0, max=100, acc=[]}).

connect() ->
    Max = case application:get_env(tachyon, statsd_buffer_size) of
              {ok, V} ->
                  V;
              _ ->
                  100
          end,
    case application:get_env(tachyon, statsd) of
        {ok, {Host, Port}} ->
            case gen_tcp:connect(Host, Port, []) of
                {ok, DB} ->
                    {ok, #statsd{enabled = true, db=DB, host=Host, port=Port,
                                 max = Max}};
                E ->
                    E
            end;
        _ ->
            #statsd{enabled = false}
    end.


put(Metric, Value, Time, Args,
    K = #statsd{enabled=true, db=DB, host=Host, port=Port, cnt=C, max=M,
                acc=Acc}) when C > M ->
    Metrics = [fmt(Metric, Value, Time, Args) | Acc],
    K1 = case gen_tcp:send(DB, Metrics) of
             {error, Reason} ->
                 gen_tcp:close(DB),
                 lager:error("[~s] Socket died with: ~p", [Host, Reason]),
                 {ok, NewDB} = gen_tcp:connect(Host, Port, [], 100),
                 K#statsd{db=NewDB};
             _ ->
                 K
         end,
    tachyon_mps:send(),
    K1#statsd{acc = [], cnt = 0};

put(Metric, Value, Time, Args, K = #statsd{enabled = true, cnt=C, acc=Acc}) ->
    tachyon_mps:send(),
    K#statsd{cnt=C+1, acc=[fmt(Metric, Value, Time, Args) | Acc]};

put(_Metric, _Value, _Time, _Args, K) ->
    K.

fmt(Metric, Value, Time, Args)  when is_integer(Value) ->
    [Metric | fmt_args(lists:reverse(Args), [$\s, integer_to_list(Value), $\s, integer_to_list(Time), $\n])];

fmt(Metric, Value, Time, Args)  when is_float(Value) ->
    [Metric | fmt_args(lists:reverse(Args), [$\s, float_to_list(Value), $\s, integer_to_list(Time), $\n])];

fmt(_, _, _, _) ->
    [].

fmt_args([{_, V}|R], Acc) when is_integer(V) ->
    fmt_args(R, [$., integer_to_list(V) | Acc]);
fmt_args([{_, V}|R], Acc) when is_float(V) ->
    fmt_args(R, [$., float_to_list(V) | Acc]);
fmt_args([{_, V}|R], Acc) when is_binary(V) ->
    fmt_args(R, [<<$., V/binary>> | Acc]);
fmt_args([{_, V}|R], Acc) when is_list(V) ->
    fmt_args(R, [$., V | Acc]);

fmt_args([], Acc) ->
    Acc.

-ifdef(TEST).

fmt_test() ->
    Fmt = fmt(<<"a.metric">>, 123, 456, [{hypervisor, <<"bla">>}, {id, 1}]),
    S = list_to_binary(Fmt),
    ?assertEqual(<<"a.metric.bla.1 123 456\n">>, S).

fmtno_arg_test() ->
    Fmt = fmt(<<"a.metric">>, 1, 2, []),
    S = list_to_binary(Fmt),
    ?assertEqual(<<"a.metric 1 2\n">>, S).

-endif.
