-module(benchmark).
-export([startServer/1, startClient/2, startClients/2, test/2]).

-import(server, [start/1]).
-import(client, [read/2, write/3, commit/2, abort/0]).

test(M, N) ->
    startServer(M),
    {AvgResTime, AbortCount, OkCount} = startClients(M, N),
    server:stop(server),
    io:format("AvgResTime: ~wms~nAbort amount: ~w~nOk amount: ~w~n", 
        [AvgResTime, AbortCount, OkCount]),
    test_finished.

startServer(N) -> 
    register(server, server:start(N)).
    
startClient(ResultsCollector, M) ->
    Start = erlang:system_time(milli_seconds),
    Handler = server:open(server),
    Algorithm = exs64,
    rand:seed(Algorithm),
    Random1 = rand:uniform(M),
    Random2 = rand:uniform(M),
    %io:format("R1: ~w~nR2: ~w~n", [Random1, Random2]),
    %Read1 = 
        client:read(Handler, Random1),
    client:write(Handler, Random1, "VAMOOO BOQUITAA CORAZAOO"),
    client:write(Handler, Random2, "Sarasa"),
    %Read2 = 
        client:read(Handler, Random1),
    %Read3 = 
        client:read(Handler, Random2),
    Status = client:commit(Handler),
    End = erlang:system_time(milli_seconds),
    %io:format("Read1: ~w~nRead2: ~w~nRead3: ~w~n", [Read1, Read2, Read3]),
    %io:format("PID ~p Status ~p ~n", [self(), Status]),
    %io:format("PID ~p ends with ~p ms. ~n", [self(), (End - Start)]),
    ResultsCollector ! { self(), Status, (End - Start) }.
    
startClients(M, N) -> 
    ResultsCollector = self(),
    [ spawn(fun() -> startClient(ResultsCollector, M) end) || _ <- lists:seq(1, N)],
    Results = [ collectRes() || _ <- lists:seq(1, N)],
    { avgResTime(Results), count(abort, Results), count(ok, Results) }.
    
collectRes() ->
    receive
        { PID, Status, RT } -> { PID, Status, RT } 
    end.

avgResTime(Results) ->
    lists:sum(lists:map(fun({ _, _, RT }) -> RT end, Results)) / length(Results).
    
count(Res, Results) ->
    length([R || { _, R, _ } <- Results, R == Res]).