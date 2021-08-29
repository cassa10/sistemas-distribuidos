-module(tests).

-export([start/0, start1/0]).

start() ->
    io:format("Hello, world!\n"),
    Input = io:get_line("Ingrese una apuesta\n"),
    Number = string:to_integer(Input),
    io:format("Number: ~w\n", [Number]),
    case Number of
        {error, _} -> io:format("No es un numero valido!\n");
        {Value, _} when Value >= 0, Value =< 36 ->  
            io:format("Value: ~w\n", [Value]);
        _ -> io:format("No es un numero valido!\n")
    end.


start1() ->
    io:format("Hello, world!\n"),
    Input = io:read("Ingrese una apuesta: "),
    case Input of
        {ok, 1} -> io:format("1!\n");
        {ok, 10} -> io:format("10!\n");
        _ -> io:format("No es un numero contemplado!\n")
    end.


%tests:start()