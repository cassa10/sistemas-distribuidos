-module(time).
-export([zero/0, inc/1, merge/2, leq/2, clock/1, update/3, safe/2, deleteNodeDown/2]).

%zero(): retorna un valor Lamport inicial (puede ser 0).
zero() -> 0.

%inc(Name, T): retorna el tiempo T incrementado en uno 
inc(T) ->
    T + 1.

%merge(Ti, Tj): unifica los dos timestamps Lamport (eso es, toma el mayor).
merge(Ti, Tj) -> 
    max(Ti, Tj).

%leq(Ti, Tj): retorna true si Ti es menor o igual a Tj.
leq(Ti, Tj) ->
    Ti =< Tj.

%clock(Nodes): retorna un reloj que pueda llevar cuenta de los nodos
clock(Nodes) ->
    lists:map(fun(Node) -> {Node, zero()} end, Nodes).

%update(Node, Time, Clock): retorna un reloj que 
% haya sido actualizado dado que hemos recibido un 
% mensaje de log de un nodo en determinado momento.
update(Node, Time, Clock) ->
    case lists:keymember(Node, 1, Clock) of
        true -> lists:keyreplace(Node, 1, Clock, {Node, Time});
        false -> [ {Node, Time} | Clock]
    end.

%safe(Time, Clock): retorna true o false si es seguro enviar el 
%mensaje de log de un evento que ocurrió en el tiempo Time dado.
safe(Time, Clock) ->
    lists:all(fun({_, SavedTime}) -> leq(Time, SavedTime) end, Clock).

deleteNodeDown(Clock, NodeDown) ->
    lists:keydelete(NodeDown, 1, Clock).