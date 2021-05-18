-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            OurTime = time:zero(),
            loop(Name, Log, Peers, Sleep, Jitter, OurTime);
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, OurTime)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            MergeTime = time:merge(time:inc(Name, OurTime), Time),
            Log ! {log, Name, MergeTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, MergeTime);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
            Selected = select(Peers),
            Time = OurTime,
            Message = {hello, random:uniform(100)},
            Selected ! {msg, Time, Message},
            jitter(Jitter),
            Log ! {log, Name, Time, {sending, Message}},
            loop(Name, Log, Peers, Sleep, Jitter, time:inc(Name, OurTime))
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
