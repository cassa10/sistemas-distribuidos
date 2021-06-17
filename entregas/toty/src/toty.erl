-module(toty).

-export([run/2, stop/0, stop/1]).

run(Jitter, Sleep) ->
    M1 = multicast:start(Jitter),
    M2 = multicast:start(Jitter),
    M3 = multicast:start(Jitter),
    M4 = multicast:start(Jitter),
    M1 ! {peers, [M2, M3, M4]},
    M2 ! {peers, [M1, M3, M4]},
    M3 ! {peers, [M1, M2, M4]},
    M4 ! {peers, [M1, M2, M3]},
    %worker:start(Name, Multicast, Seed, Sleep)
    register(w1, worker:start("PepeGrillo", M1, 34, Sleep)),
    register(w2, worker:start("Pinorcho", M2, 37, Sleep)),
    register(w3, worker:start("Pikarchu", M3, 43, Sleep)),
    register(w4, worker:start("Sarasa", M4, 72, Sleep)),
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