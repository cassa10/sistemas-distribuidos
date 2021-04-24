-module(consumer).
-export([start/1, stop/0]).

start(Producer) ->
    Consumer = spawn(fun() -> init(Producer) end),
    register(consumer, Consumer).

init(Producer) ->
    Monitor = monitor(process, Producer),
    Producer ! {hello, self()},
    consumer(0, Producer, Monitor).

consumer(N, Producer, Monitor) ->
    receive
        {ping, M} ->
            validateNumbers(N, M),
            consumer(N+1, Producer, Monitor);
        bye ->
            Producer ! consumerDie;
        {'DOWN', Monitor, process, Object, Info} ->
            io:format("~w died; ~w~n", [Object, Info]),
            consumer(N, Producer, Monitor)
    end.

stop() ->
    consumer ! bye.

validateNumbers(N, M) -> 
    if
      N =:= M -> io:format("ping ~p~n", [M]);
      M > N -> io:format("el numero fue mas alto M:~p N:~p~n", [M, N]);
      true  -> io:format("error unhandled ~n")
    end.
