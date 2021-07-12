-module(tests).

-export([start/0]).

start() ->
    io:format("Hello, world!\n"),
    Input = io:get_line("Ingrese una apuesta\n"),
    Number = string:to_integer(Input),
    io:format("Number: ~w\n", [Number]),
    case Number of
        {error, _} -> io:format("No es un numero valido!\n");
        {Value, _} when Value > 0 ->  
            io:format("Value: ~w\n", [Value]);
        _ -> io:format("No es un numero valido!\n")
    end.

%tests:start()