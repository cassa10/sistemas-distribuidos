-module(worker).
-export([start/4]).

-define(deadlock, 4000).

start(Name, Multicast, Seed, Sleep) ->
    spawn(fun() -> init(Name, Multicast, Seed, Sleep) end).
    
init(Name, Multicast, Seed, Sleep) ->
    Multicast ! {start, self()},
    Gui = spawn(gui, init, [Name]),
    random:seed(Seed, Seed, Seed),
    State = { 0, 0, 0 },
    worker(Multicast, State, Sleep, Gui),
    Gui ! stop.

worker(Multicast, State, Sleep, Gui) ->
    Wait = random:uniform(Sleep),
    receive
        stop ->
            stop
    after Wait ->
        N = random:uniform(20),
        Multicast ! {send, N},
        NewState = draw(N, State, Gui),
        worker(Multicast, NewState, Sleep, Gui)
    end.

draw(N, State, Gui) ->
    receive
        {response, N} -> 
            NewState = rotate(State, N),
            Gui ! {draw, NewState},
            NewState;
        {response, M} -> 
            NewState = rotate(State, M),
            Gui ! {draw, NewState},
            draw(N, NewState, Gui)
    end.

rotate({R, G, B}, N) -> 
    {G, B, (R+N) rem 256}.