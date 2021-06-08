-module(lock2).
-export([start/1]).

start(Id) ->
    spawn(fun() -> init(Id) end).

init(Id) ->
    receive
        { peers, Peers } ->
        open(Id, Peers);
    stop ->
        ok
    end.

open(Id, Nodes) ->
    receive
        {take, Master} ->
            Refs = requests(Id, Nodes),
            wait(Nodes, Id, Master, Refs, []);
        {request, From, _, Ref} ->
            From ! {ok, Ref},
            open(Id, Nodes);
        stop ->
            ok
    end.
    
requests(Id, Nodes) ->
    lists:map(fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), Id, R}, 
        R 
    end, Nodes).
    
wait(Id, Nodes, Master, [], Waiting) ->
    Master ! taken,
    held(Id, Nodes, Waiting);

wait(Id, Nodes, Master, Refs, Waiting) ->
    receive
        {request, From, FromId, Ref} when FromId >= Id ->
            wait(Id, Nodes, Master, Refs, [{From, Ref}|Waiting]);
        {request, From, FromId, Ref} when FromId < Id ->
            From ! {ok, Ref},
            wait(Id, Nodes, Master, Refs, Waiting);
        {ok, Ref} ->
            Refs2 = lists:delete(Ref, Refs),
            wait(Id, Nodes, Master, Refs2, Waiting);
        release ->
            ok(Waiting),
            open(Id, Nodes)
    end.

ok(Waiting) ->
    lists:foreach(fun({F,R}) -> 
        F ! {ok, R} 
    end, Waiting).
    
held(Id, Nodes, Waiting) ->
    receive
        { request, From, _, Ref } ->
            held(Id, Nodes, [ {From, Ref} | Waiting ]);
        release ->
            ok(Waiting),
            open(Id, Nodes)
    end.
