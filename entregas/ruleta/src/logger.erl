-module(logger).

-export([log/1, logf/2]).

log(Message) -> 
    io:format("[~s] | ", [nowf()]),
    io:format(Message).

logf(Message, Params) -> 
    io:format("[~s] | ", [nowf()]),
    io:format(Message, Params).

%Return time now like "yyyy-mm-ddThh:mm:ss.zZ"
nowf() -> 
    Now = erlang:timestamp(),
    {_, _, MicroSecs} = Now,
    Ms = round(MicroSecs / 1000),
    {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_local_time(Now),
    Str = io_lib:format("~w-~w-~wT~w:~w:~w.~wZ",[Year, Month, Day, Hour, Minute, Second, Ms]),
    Str.