-module(time).
-export([zero/0, inc/1, merge/2, leq/2, eq/2]).

zero() ->
    0.

inc(T) ->
    T+1.

merge(Ta, Tb) ->
    max(Ta, Tb).

leq(Ta, Tb) ->
    Ta =< Tb.

eq(Ta, Tb) ->
   Ta == Tb.