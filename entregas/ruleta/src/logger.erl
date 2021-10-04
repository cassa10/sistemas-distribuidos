-module(logger).

-export([log/1, logf/2, logf/1]).

log(Message) -> 
    io:format("[~s] | ", [timeNowf()]),
    io:format(Message),
    io:format("~n").

logf(Message) -> 
    log(Message).

logf(Message, Params) -> 
    io:format("[~s] | ", [timeNowf()]),
    io:format(Message, Params),
    io:format("~n").

%Return time now like "yyyy-mm-ddThh:mm:ss.zZ"
timeNowf() -> 
    Now = erlang:timestamp(),
    {_, _, MicroSecs} = Now,
    Ms = round(MicroSecs / 1000),
    {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_local_time(Now),
    Str = io_lib:format("~w-~w-~wT~w:~w:~w.~wZ",[Year, Month, Day, Hour, Minute, Second, Ms]),
    Str.