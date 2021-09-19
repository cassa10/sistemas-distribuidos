-module(testClient).

-export([apostar/3, nowf/0]).

%Id es un atom, identificador del nodo para identificar el pid.
%Load Balancer = {'Id del proceso', Node()}
%Apuesta es un int
apostar(Id, LoadBalancer, Apuesta) ->
    register(Id, self()),
    io:format("Time: ~s~n",[nowf()]),
    io:format("Cliente with Id ~w mandando apuesta ~w~n", [Id, Apuesta]),
    LoadBalancer ! {apostar, {Id, node()}, Apuesta},
    receive
        {data, Data} -> io:format("Data recieved: ~w~n",[Data])
    end.


%Return time now like "yyyy-mm-ddThh:mm:ss.zZ"
nowf() -> 
    Now = erlang:timestamp(),
    {_, _, MicroSecs} = Now,
    Ms = round(MicroSecs / 1000),
    {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_local_time(Now),
    Str = io_lib:format("~w-~w-~wT~w:~w:~w.~wZ",[Year, Month, Day, Hour, Minute, Second, Ms]),
    Str.