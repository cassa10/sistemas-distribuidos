-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = time:clock(Nodes),
    Queue = queue:new(),
    loop(Queue, Clock).

loop(Queue, Clock) ->
    receive
        {log, From, Time, Msg} ->
            UpdatedClock = time:update(From, Time, Clock),
            UpdatedQueue = case time:safe(Time, UpdatedClock) of
                true -> 
                    log(From, Time, Msg),
                    logAllSafeMsg(Time, Queue),
                    %return a queue
                    filterUnsafeMsg(Time, Queue);
                false ->
                    queue:in({From, Time, Msg}, Queue)
            end,
            loop(UpdatedQueue, UpdatedClock);
         stop ->
            logAll(Queue),
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

logAllSafeMsg(Time, Queue) ->
    SafeMsgs = queue:filter(fun({_, QT, _}) -> time:leq(QT, Time) end, Queue),
    logAll(SafeMsgs).

filterUnsafeMsg(TimeToFilter, Queue) ->
    queue:filter(fun({ _, Time, _}) -> not time:leq(Time, TimeToFilter) end, Queue).

sortQueue(Queue) ->
    lists:sort(fun({_, T1, _}, {_, T2, _}) -> time:leq(T1, T2) end, queue:to_list(Queue)).

logAll(Queue) ->
    lists:foreach(fun({ From, T, Msg}) -> log(From, T, Msg) end, sortQueue(Queue)).
