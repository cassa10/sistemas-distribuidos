-module(pingpong).

-export([start_ping/2, start_pong/0,  ping/2, pong/0]).

ping(0, P_Node) ->
    {pong, P_Node} ! finished,
    io:format("~p se fue~n", [node()]);

ping(N, P_Node) ->
    %io:format("~p manda ping~n", [node()]),
    {pong, P_Node} ! {ping, self(), node()},
    receive
        pong ->
            io:format("~p recibe pong~n", [P_Node])
    end,
    ping(N - 1, P_Node).

pong() ->
    receive
        finished ->
            io:format("FIN :( ~n", []);
        {ping, P, Node} ->
            io:format("~p recibe ping~n", [Node]),
            %io:format("~p manda pong~n", [node()]),
            P ! pong,
            pong()
    end.

start_pong() ->
    register(pong, spawn(pingpong, pong, [])).

start_ping(N, P_Node) ->
    spawn(pingpong, ping, [N, P_Node]).