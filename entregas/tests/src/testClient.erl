-module(testClient).

-export([apostar/2]).

%Apuesta es un int
apostar(LoadBalancer, Apuesta) ->
    io:format("Cliente mandando apuesta "+ Apuesta +"\n"),
    LoadBalancer ! {apuesta, self(), Apuesta}.