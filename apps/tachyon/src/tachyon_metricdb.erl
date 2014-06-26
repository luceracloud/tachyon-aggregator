%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_metricdb).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([connect/0, put/5]).
-ignore_xref([put/5]).
-record(metricdb, {enabled, db, host, port, acc = <<>>}).

-define(BUFFER_SIZE, 7000).

connect() ->
    case application:get_env(tachyon, metricdb) of
        {ok, {Host, Port}} ->
            case gen_udp:open(0, [{active, false}, binary]) of
                {ok, DB} ->
                    {ok, #metricdb{enabled = true, db=DB, host=Host, port=Port}};
                E ->
                    E
            end;
        _ ->
            {ok, #metricdb{enabled = false}}
    end.

put(Metric, Value, Time, Args,
    K = #metricdb{enabled=true, acc=Acc})
  when byte_size(Acc) < ?BUFFER_SIZE ->
    tachyon_mps:send(),
    M = fmt(Metric, Value, Time, Args),
    K#metricdb{acc = <<Acc/binary, M/binary>>};

put(Metric, Value, Time, Args,
    K = #metricdb{enabled=true, db=DB, host=Host, port=Port, acc=Acc}) ->
    tachyon_mps:send(),
    M = fmt(Metric, Value, Time, Args),
    case gen_udp:send(DB, Host, Port, <<Acc/binary, M/binary>>) of
        {error, _Reason} ->
            gen_udp:close(DB),
            %%lager:error("[~s] Socket died with: ~p", [Host, Reason]),
            {ok, NewDB} = gen_udp:open(0, [{active, false}, binary]),
            K#metricdb{db=NewDB, acc = <<>>};
        _ ->
            K#metricdb{acc = <<>>}
    end;

put(_Metric, _Value, _Time, _Args, K) ->
    K.

fmt(Metric, Value, Time, Args)  when is_integer(Value) ->
    M = fmt_args(Args, Metric),
    MS = byte_size(M),
    <<0, Time:64/integer, MS:16/integer, M/binary, 9:16, 1, Value:64/signed-integer>>;

fmt(Metric, Value, Time, Args)  when is_float(Value) ->
    M = fmt_args(Args, Metric),
    MS = byte_size(M),
    <<0, Time:64/integer, MS:16/integer, M/binary, 9:16, 2, Value:64/float>>.

i2b(I) ->
    list_to_binary(integer_to_list(I)).
f2b(I) ->
    list_to_binary(float_to_list(I)).

fmt_args([{_, V}|R], Acc) when is_integer(V) ->
    fmt_args(R, <<Acc/binary, $., (i2b(V))/binary>>);
fmt_args([{_, V}|R], Acc) when is_float(V) ->
    fmt_args(R, <<Acc/binary, $., (f2b(V))/binary>>);
fmt_args([{_, V}|R], Acc) when is_binary(V) ->
    fmt_args(R, <<Acc/binary, $., V/binary>>);
fmt_args([{_, V}|R], Acc) when is_list(V) ->
    fmt_args(R, <<Acc/binary, $., (list_to_binary(V))/binary>>);

fmt_args([], Acc) ->
    Acc.
