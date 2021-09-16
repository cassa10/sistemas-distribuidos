-module(testClient).

-export([apostar/2]).

%Load Balancer = {'Id del proceso', Node()}
%Apuesta es un int
apostar(LoadBalancer, Apuesta) ->
    io:format("Cliente mandando apuesta ~w~n", [Apuesta]),
    LoadBalancer ! {apostar, {self(), node()}, Apuesta},
    receive
        {Data} -> io:format("Data recieved: ~w~n",[Data])
    end.