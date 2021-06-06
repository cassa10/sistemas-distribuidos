-module(muty).

-export([start/3, stop/0]).

start(Lock, Sleep, Work) ->
    L1 = apply(Lock, start, [1]),
    L2 = apply(Lock, start, [2]),
    L3 = apply(Lock, start, [3]),
    L4 = apply(Lock, start, [4]),
    L1 ! {peers, [L2, L3, L4]},
    L2 ! {peers, [L1, L3, L4]},
    L3 ! {peers, [L1, L2, L4]},
    L4 ! {peers, [L1, L2, L3]},
    register(w1, worker:start("John", L1, 34, Sleep, Work)),
    register(w2, worker:start("Ringo", L2, 37, Sleep, Work)),
    register(w3, worker:start("Paul", L3, 43, Sleep, Work)),
    register(w4, worker:start("George", L4, 72, Sleep, Work)),
    ok.

stop() ->
    stop(w1), stop(w2), stop(w3), stop(w4).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

