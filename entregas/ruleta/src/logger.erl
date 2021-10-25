-module(logger).

-export([log/3, logf/4, logf/3]).

log(Logger, Time, Message) -> 
    Log_date = io_lib:format("[~s] | ",[timeNowf()]),
    Log_msg = Log_date ++ Message ++ "~n",
    io:format(Log_msg),
    sendToLogger(Logger, Time, Log_msg).

logf(Logger, Time, Message) -> 
    log(Logger, Time, Message).

logf(Logger, Time, Message, Params) ->
    Log_date = io_lib:format("[~s] | ",[timeNowf()]),
    Log_msg = Log_date ++ io_lib:format(Message ++ "~n", Params),
    io:format(Log_msg),
    sendToLogger(Logger, Time, Log_msg).

sendToLogger(Logger, Time, Log_msg) ->
    Logger ! {log, node(), Time, Log_msg}.

%Return time now like "yyyy-mm-ddThh:mm:ss.zZ"
timeNowf() -> 
    Now = erlang:timestamp(),
    {_, _, MicroSecs} = Now,
    Ms = round(MicroSecs / 1000),
    {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_local_time(Now),
    Str = io_lib:format("~w-~w-~wT~w:~w:~w.~wZ",[Year, Month, Day, Hour, Minute, Second, Ms]),
    Str.