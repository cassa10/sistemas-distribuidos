-module(lock3).
-export([start/1]).

start(Id) ->
    spawn(fun() -> init(Id) end).

init(_) ->
    receive
        { peers, Peers } ->
        Time = time:zero(),
        open(Time, Peers);
    stop ->
        ok
    end.

open(Time, Nodes) ->
    receive
        {take, Master} ->
            UpdateTime = Time + 1,
            Refs = requests(UpdateTime, Nodes),
            wait(UpdateTime, Nodes, Master, Refs, []);
        {request, TimeFrom, From, Ref} ->
            From ! {ok, Ref},
            UpdateTime = time:merge(Time, TimeFrom),
            open(UpdateTime, Nodes);
        stop ->
            ok
    end.
    
requests(Time, Nodes) ->
    lists:map(fun(P) -> 
        R = make_ref(),
        P ! {request, Time, self(), R}, 
        R 
    end, Nodes).
    
wait(Time, Nodes, Master, [], Waiting) ->
    Master ! taken,
    held(Time, Nodes, Waiting);

wait(Time, Nodes, Master, Refs, Waiting) ->
    receive
        {request, TimeFrom, From, Ref} ->
            UpdateTime = time:merge(Time, TimeFrom),
            case time:leq(Time, TimeFrom) of
                true  -> 
                    wait(UpdateTime, Nodes, Master, Refs, [{From, Ref}|Waiting]);
                false -> 
                    From ! {ok, Ref},
                    wait(UpdateTime, Nodes, Master, Refs, Waiting)
            end;
        {ok, Ref} ->
            Refs2 = lists:delete(Ref, Refs),
            wait(Time, Nodes, Master, Refs2, Waiting);
        release ->
            ok(Waiting),
            open(Time, Nodes)
    end.

ok(Waiting) ->
    lists:foreach(fun({F,R}) -> 
        F ! {ok, R} 
    end, Waiting).
    
held(Time, Nodes, Waiting) ->
    receive
        {request, TimeFrom, From, Ref} ->
            UpdateTime = time:merge(Time, TimeFrom),
            held(UpdateTime, Nodes, [ {From, Ref} | Waiting ]);
        release ->
            ok(Waiting),
            open(Time, Nodes)
    end.
