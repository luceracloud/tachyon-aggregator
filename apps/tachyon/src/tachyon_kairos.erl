-module(tachyon_kairos).

-export([connect/1, put/5]).
-record(kairosdb, {db, host, port}).

connect({Host, Port}) ->
    case gen_tcp:connect(Host, Port, [{packet, line}]) of
        {ok, DB} ->
            {ok, #kairosdb{db=DB, host=Host, port=Port}};
        E ->
            E
    end.

put(Metric, Value, Time, Args, K = #kairosdb{db=DB, host=Host, port=Port}) ->
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
    DB1.

fmt(Metric, Value, Time, Args) ->
    {Fmt, Vals} =
        lists:foldl(fun({K, V}, {S, A}) when is_binary(V)->
                            {S ++ " ~p=~s", [V, K | A]};
                       ({K, V}, {S, A}) when is_number(V) ->
                            {S ++ " ~p=~p", [V, K | A]};
                       (_, Acc) ->
                            Acc
                    end, {"put ~s ~p ~p", [Value, Time, Metric]}, Args),
    io_lib:format(Fmt ++ "~n", lists:reverse(Vals)).
