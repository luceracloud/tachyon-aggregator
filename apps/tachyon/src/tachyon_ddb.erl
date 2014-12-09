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

-define(OPTS, [{active, false}, binary, {packet, 4}]).

-record(ddb, {enabled, hosts, rhosts=[], acc = <<>>,
              buffer_size=4096, bucket = <<"tachyon">>}).


connect() ->
    case application:get_env(tachyon, ddb_ips) of
        {ok, List} ->
            {ok, BufferSize} = application:get_env(tachyon, ddb_buffer_size),
            {ok, BucketS} = application:get_env(tachyon, ddb_bucket),
            Bucket = list_to_binary(BucketS),
            List1 = [begin
                         {ok, Sock} = gen_tcp:connect(Host, Port, ?OPTS),
                         {Host, Port, Sock}
                     end ||
                        {Host, Port} <- List],
            {ok, #ddb{enabled=true, hosts=List1, bucket=Bucket,
                      acc=dproto_udp:encode_header(Bucket),
                      buffer_size=BufferSize}};
        _ ->
            {ok, #ddb{enabled = false}}
    end.

put(Metric, Value, Time, Args,
    K = #ddb{enabled=true, acc=Acc, buffer_size = BS})
  when byte_size(Acc) < BS ->
    tachyon_mps:send(),
    M = fmt(Metric, Value, Time, Args),
    K#ddb{acc = <<Acc/binary, M/binary>>};

put(Metric, Value, Time, Args,
    K = #ddb{enabled=true, hosts=[], rhosts=Hosts}) ->
    put(Metric, Value, Time, Args,
        K#ddb{hosts=lists:reverse(Hosts), rhosts=[]});

put(Metric, Value, Time, Args,
    K = #ddb{enabled=true, hosts=[{Host, Port, Sock} | Hosts], rhosts=HostsR,
             acc=Acc, bucket=Bucket}) ->
    tachyon_mps:send(),
    M = fmt(Metric, Value, Time, Args),
    K1 = K#ddb{acc = dproto_udp:encode_header(Bucket)},
    Sock1 = case gen_tcp:send(Sock, <<Acc/binary, M/binary>>) of
                {error, Reason} ->
                    gen_tcp:close(Sock),
                    lager:error("[~s] Socket died with: ~p", [Host, Reason]),
                    {ok, NewSock} = gen_tcp:connect(Host, Port, ?OPTS),
                    NewSock;
                _ ->
                    Sock

            end,
    K1#ddb{hosts=Hosts, rhosts=[{Host, Port, Sock1} | HostsR]};

put(_Metric, _Value, _Time, _Args, K) ->
    K.

fmt(Metric, Value, Time, Args)  when is_integer(Value) ->
    dproto_udp:encode_points(fmt_args(Args, Metric), Time, Value);

fmt(Metric, Value, Time, Args)  when is_float(Value) ->
    dproto_udp:encode_points(fmt_args(Args, Metric), Time, round(Value)).

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

i2b(I) ->
    list_to_binary(integer_to_list(I)).
f2b(I) ->
    list_to_binary(float_to_list(I)).
