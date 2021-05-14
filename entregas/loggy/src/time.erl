-module(test).
-export([zero/0, inc/2, merge/2, leq/2]).

%zero(): retorna un valor Lamport inicial (puede ser 0).
zero() -> 0.

%inc(Name, T): retorna el tiempo T incrementado en uno 
%(probablemente ignoremos el Name, pero lo usaremos más adelante).
inc(Name, T) ->
    T + 1.

%merge(Ti, Tj): unifica los dos timestamps Lamport (eso es, toma el mayor).
merge(Ti, Tj) -> 
    max(Ti, Tj).

%leq(Ti, Tj): retorna true si Ti es menor o igual a Tj.
leq(Ti, Tj) ->
    Ti =< Tj.