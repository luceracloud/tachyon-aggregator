%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Lucera Financial Infrastructures
%%% @doc
%%%
%%% @end
%%% Created : 26 Sep 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------

-module(tachyon_statistics).

-include("tachyon_statistics.hrl").

-export([avg_update/2,
         avg_analyze/3,
         update_and_analyze/3]).

-ignore_xref([avg_update/2,
              avg_analyze/3]).

-define(UPD(Avg, N, Decay),
        (Avg*Decay) + (N*(1-Decay))).

avg_update(Met = #running_avg{
                    avg = Avg0,
                    min_iter = MinIter,
                    quick_decay = QDecay,
                    decay = Decay
                   }, V) ->
    D = Avg0 - V,
    Dsq = D*D,
    Iter = Met#running_avg.itteration,
    Var0 = Met#running_avg.var,
    {Var, Avg} =
        case Iter of
            _I0 when _I0 < MinIter ->
                {?UPD(Var0, Dsq, QDecay),
                 ?UPD(Avg0, V, QDecay)};
            _ ->
                {?UPD(Var0, Dsq, Decay),
                 ?UPD(Avg0, V, Decay)}
        end,
    Std = math:sqrt(Var),
    Dist = if
               Std > 0 ->
                   abs(D/Std);
               true ->
                   0.0
           end,
    Met#running_avg{
      var = Var, std = Std, avg = Avg,
      itteration = Iter + 1,
      dist = Dist
     }.

avg_analyze(M = #running_avg{itteration = Iter, min_iter = MinIter}, _, _)
  when Iter =< MinIter ->
    {ok, M};
avg_analyze( Met = #running_avg{
                      std = Std, dist = Dist, avg = Avg,
                      info = Info0, warn = Warn0, error = Error0
                     }, V, T) ->
    RelDist = if
                  Std > 0 -> Dist/Std;
                  true    -> 0
              end,
    Diff = Avg - V,
    %% We don't ant to have alerts if the total delta is less then 1%
    Delta = if
                Avg > 0 -> Diff/Avg;
                true    -> 0
            end,
    MinDelta = 0.02,
    Msg = "Last value ~.2f was ~.2f(~.2f%) std diviations from avg ~.2f.",
    Args = [V*1.0, RelDist, Delta*100, Avg],
    {Reply, Info, Warn, Error} =
        if
            RelDist > T*3,
            Delta > MinDelta ->
                {{error, Msg, Args}, Info0, Warn0, Error0 + 1};
            RelDist > T*2,
            Delta > MinDelta ->
                {{warn, Msg, Args}, Info0, Warn0 + 1, 0};
            RelDist > T,
            Delta > MinDelta ->
                {{info, Msg, Args}, Info0 + 1, 0, 0};
            true ->
                {ok, 0, 0, 0}
        end,
    {Reply, Met#running_avg{info = Info, warn = Warn, error = Error}}.


update_and_analyze(Met = #running_avg{
                            avg = Avg0,
                            min_iter = MinIter,
                            quick_decay = QDecay,
                            decay = Decay,
                            itteration = Iter,
                            var = Var0,
                            info = Info0, warn = Warn0, error = Error0
                           }, V, T) ->
    D = Avg0 - V,
    Dsq = D*D,
    {Var, Avg} =
        case Iter of
            _I0 when _I0 < MinIter ->
                {?UPD(Var0, Dsq, QDecay),
                 ?UPD(Avg0, V, QDecay)};
            _ ->
                {?UPD(Var0, Dsq, Decay),
                 ?UPD(Avg0, V, Decay)}
        end,
    Std = math:sqrt(Var),
    Dist = if
               Std > 0 ->
                   abs(D/Std);
               true ->
                   0.0
           end,
    Met1 = Met#running_avg{
             var = Var, std = Std, avg = Avg,
             itteration = Iter + 1,
             dist = Dist
            },
    case Iter of
        _I1 when _I1 < MinIter ->
            {ok, Met1};
        _ ->
            MinDelta = 0.02,
            RelDist = if
                          Std > 0 -> Dist/Std;
                          true    -> 0
                      end,
            Delta = if
                        Avg > 0 -> D/Avg;
                        true    -> 0
                    end,
            Msg = "Last value ~.2f was ~.2f(~.2f%) std diviations from avg ~.2f.",
            Args = [V*1.0, RelDist, Delta*100, Avg],
            {Reply, Info, Warn, Error} =
                if
                    RelDist > T*3,
                    Delta > MinDelta ->
                        {{error, Msg, Args}, Info0, Warn0, Error0 + 1};
                    RelDist > T*2,
                    Delta > MinDelta ->
                        {{warn, Msg, Args}, Info0, Warn0 + 1, 0};
                    RelDist > T,
                    Delta > MinDelta ->
                        {{info, Msg, Args}, Info0 + 1, 0, 0};
                    true ->
                        {ok, 0, 0, 0}
                end,
            {Reply, Met#running_avg{info = Info, warn = Warn, error = Error}}
    end.
