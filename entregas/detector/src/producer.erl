-module(producer).
-export([start/1, stop/0, crash/0]).

start(Delay) ->
    Producer = spawn(fun() -> init(Delay) end),
    register(producer, Producer).

stop() ->
    producer ! stop.

crash() ->
    producer ! crash.

init(Delay) ->
    receive
        {hello, Consumer} ->
            producer(Consumer, 0, Delay);
        stop ->
            ok
    end.

waitConsumer(N, Delay) ->
    receive
        {hello, Consumer} ->
            producer(Consumer, N, Delay); 
        stop ->
            ok
        after Delay -> % se reinicia el delay y se retrasa un ping
            waitConsumer(N+1, Delay)
    end.

producer(Consumer, N, Delay) ->
    receive
        stop ->
            Consumer ! bye;
        crash ->
            42/0; %% this will give you a warning, but it is ok
        consumerDie ->
            waitConsumer(N, Delay)
        after Delay ->
            Consumer ! {ping, N},
            producer(Consumer, N+1, Delay)
    end.
