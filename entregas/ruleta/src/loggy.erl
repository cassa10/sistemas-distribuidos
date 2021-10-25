-module(loggy).
-export([start/2, stop/1]).

start(Id, Nodes) ->
    Pid = self(),
    register(Id, Pid),
    io:format("Start loggy logger with id: ~w, pid: ~w", [Id, Pid]),
    init(Nodes).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = time:clock(Nodes),
    Queue = [],
    monitorNodes(Nodes),
    loop(Queue, Clock).

loop(Queue, Clock) ->
    receive
        {log, From, Time, Msg} ->
            UpdatedClock = time:update(From, Time, Clock),
            UpdatedQueue = [{From, Time, Msg} | Queue],
            io:format("DEBUG - From: ~w, UpdatedClock: ~w, isTimeSafe: ~w ~n", [From, UpdatedClock, time:safe(Time, UpdatedClock)]),
            FilteredQueue = case time:safe(Time, UpdatedClock) of
                true -> 
                    logAllSafeMsg(Time, UpdatedQueue),
                    filterUnsafeMsg(Time, UpdatedQueue);
                false ->
                    UpdatedQueue
                end,
            loop(FilteredQueue, UpdatedClock);
        {nodedown, Node} -> 
            io:format("Nodo caido ~w, removiendo el nodo del clock~n", [Node]),
            UpdatedClock = time:deleteNodeDown(Clock, Node),
            loop(Queue, UpdatedClock);
        stop ->
        logAll(Queue),
        ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~s~n", [Time, From, Msg]).

logAllSafeMsg(Time, Queue) ->
    SafeMsgs = lists:filter(fun({_, QT, _}) -> time:leq(QT, Time) end, Queue),
    logAll(SafeMsgs).

filterUnsafeMsg(TimeToFilter, Queue) ->
    lists:filter(fun({ _, Time, _}) -> not time:leq(Time, TimeToFilter) end, Queue).

sortQueue(Queue) ->
    lists:sort(fun({_, T1, _}, {_, T2, _}) -> orderByLessTimeAndUntieSending(T1, T2) end, Queue).

logAll(Queue) ->
    lists:foreach(fun({ From, T, Msg}) -> log(From, T, Msg) end, sortQueue(Queue)).

orderByLessTimeAndUntieSending(T1, T2) ->
    time:leq(T1, T2).

monitorNodes(Nodes) ->
    lists:foreach(fun(Node) -> monitor_node(Node, true) end, Nodes).