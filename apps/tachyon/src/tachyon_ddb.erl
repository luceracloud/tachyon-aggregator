%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_ddb).

-include_lib("dproto/include/dproto.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([connect/0, put/5]).
-ignore_xref([put/5]).
-record(ddb, {enabled, db, host, port, acc = <<>>, port_inc = 0,
              buffer_size=4096, port_num=30, bucket = <<"tachyon">>}).

-define(BUFFER_SIZE, 4096).

connect() ->
    case application:get_env(tachyon, ddb_ip) of
        {ok, {Host, Port}} ->
            {ok, BufferSize} = application:get_env(tachyon, ddb_buffer_size),
            {ok, NumPorts} = application:get_env(tachyon, ddb_num_ports),
            {ok, Bucket} = application:get_env(tachyon, ddb_bucket),
            case gen_udp:open(0, [{active, false}, binary]) of
                {ok, DB} ->
                    {ok, #ddb{enabled=true, db=DB, host=Host, port=Port,
                              bucket=list_to_binary(Bucket),
                              buffer_size=BufferSize, port_num=NumPorts}};
                E ->
                    E
            end;
        _ ->
            {ok, #ddb{enabled = false}}
    end.

put(Metric, Value, Time, Args,
    K = #ddb{enabled=true, acc=Acc, bucket=Bucket})
  when byte_size(Acc) < ?BUFFER_SIZE ->
    tachyon_mps:send(),
    M = fmt(Bucket, Metric, Value, Time, Args),
    K#ddb{acc = <<Acc/binary, M/binary>>};

put(Metric, Value, Time, Args,
    K = #ddb{enabled=true, db=DB, host=Host, port=Port, acc=Acc, port_inc=I,
             port_num=Max, bucket=Bucket}) ->
    tachyon_mps:send(),
    M = fmt(Bucket, Metric, Value, Time, Args),
    I1 = (I + 1) rem Max,
    K1 = K#ddb{port_inc = I1},
    case gen_udp:send(DB, Host, Port + I, <<Acc/binary, M/binary>>) of
        {error, Reason} ->
            gen_udp:close(DB),
            lager:error("[~s] Socket died with: ~p", [Host, Reason]),
            {ok, NewDB} = gen_udp:open(0, [{active, false}, binary]),
            K1#ddb{db=NewDB, acc = <<>>};
        _ ->
            K1#ddb{acc = <<>>}
    end;

put(_Metric, _Value, _Time, _Args, K) ->
    K.

fmt(Bucket, Metric, Value, Time, Args)  when is_integer(Value) ->
    M = fmt_args(Args, Metric),
    <<?PUT, (dproto_udp:encode_points(Bucket, M, Time, Value))/binary>>;

fmt(Bucket, Metric, Value, Time, Args)  when is_float(Value) ->
    M = fmt_args(Args, Metric),
    <<?PUT, (dproto_udp:encode_points(Bucket, M, Time, round(Value)))/binary>>.

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
