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
            UpdatedQueue = queue:in({From, Time, Msg}, Queue),
            FilteredQueue = case time:safe(Time, UpdatedClock) of
                true -> 
                    logAllSafeMsg(Time, UpdatedQueue),
                    filterUnsafeMsg(Time, UpdatedQueue);
                false ->
                    UpdatedQueue
            end,
            loop(FilteredQueue, UpdatedClock);
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
    lists:sort(fun({_, T1, Msg1}, {_, T2, _}) -> orderByLessTimeAndUntieSending(T1, T2, Msg1) end, queue:to_list(Queue)).

logAll(Queue) ->
    lists:foreach(fun({ From, T, Msg}) -> log(From, T, Msg) end, sortQueue(Queue)).

orderByLessTimeAndUntieSending(T1, T2, Msg1) ->
    case time:leq(T1, T2) of
        true -> untie(T1, T2, Msg1);
        false -> false
    end.

isNotReceived(Msg) ->
    case Msg of
        {received, _} -> false;
        _ -> true
    end.

untie(T1, T2, Msg1) ->
    case T1 == T2 of
        true -> isNotReceived(Msg1);
        false -> true
    end.