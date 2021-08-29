-module(testMain).

-export([start/0]).

% TODO: INFORME
%CASO POCO PROBABLE: Inconsistencia cuando el cliente le pega al Load Balancer y este ultimo chequea que el Server esta bien. 
%Cuando el Server va a hacer una replica del mismo a sus slaves, y cae antes de hacerlo, se pierde ese dato.


%IDEA TEST:
    % Cliente le pega a Load Balancer, esto va al Server1 (MAIN), para guardar un dato.
    % Cliente le pega a Load Balancer, esto va al Server1 (MAIN), para guardar otro dato.
    % Simulamos que se cae Server1
    % Load Balancer se encarga de poner MAIN a Server2 o Server3 ni idea el algoritmo.
    % El Cliente recibe la respuesta correctamente.
start() ->
    io:format("Test Main\n"),
    Server1 = testServer:start(1, true),
    Server2 = testServer:start(2, false),
    Server3 = testServer:start(2, false),
    Server1 ! {peers, [Server2, Server3]},
    Server2 ! {peers, [Server1, Server3]},
    Server3 ! {peers, [Server1, Server2]},
    LoadBalancer = testLoadBalancer:start(Server1, [Server1, Server2, Server3]),
    testClient:apostar(LoadBalancer, 100).