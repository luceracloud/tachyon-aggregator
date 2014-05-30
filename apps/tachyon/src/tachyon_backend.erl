%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(tachyon_backend).

-record(tachyon_backend, {backends}).

-export([init/0, put/5]).

init() ->
    {ok, DB} = tachyon_kairos:connect(),
    {ok, Statsd} = tachyon_statsd:connect(),
    {ok, #tachyon_backend{backends=[{tachyon_kairos, DB}, {tachyon_statsd, Statsd}]}}.

put(Metric, Value, Time, Args, Backend = #tachyon_backend{backends=Backends}) ->
    Backends1 = [{Mod, Mod:put(Metric, Value, Time, Args, DB)} ||
                    {Mod, DB} <- Backends],
    Backend#tachyon_backend{backends = Backends1}.
