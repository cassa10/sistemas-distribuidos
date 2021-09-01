-module(testClient).

-export([apostar/2]).

%Load Balancer = {'Id del proceso', Node()}
%Apuesta es un int
apostar(LoadBalancer, Apuesta) ->
    io:format("Cliente mandando apuesta "+ Apuesta +"\n"),
    LoadBalancer ! {apuesta, {self(), node()}, Apuesta},
    receive
        {Data} -> io:format("Data recieved: "+ Data +"\n")
    end.