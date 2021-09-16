-module(testServer).

-export([init/1, start/2, waitMaster/1, slaveMode/3, startMaster/1, esperarApuestas/2, imprimirApuestas/2, backup/2, cambioEstado/2]).

% Id :: atom
start(Id, Nodes) ->
    Pid = self(),
    io:format("Start server with ~w id and ~w as pid.~n", [Id, Pid]),
    register(Id, Pid),
    init(Nodes).

init(Nodes) ->
    waitMaster(Nodes).

waitMaster(Peers) ->
    receive
        master -> 
            io:format("received master~n"),
            startMaster(Peers);
        slave  -> 
            io:format("received slave~n"),
            slaveMode(Peers, esperarApuestas, [])
    end.

slaveMode(Peers, EstadoMaster, DataMaster) ->
    io:format("from slave mode"),
    receive
        {cambioEstado, NuevoEstado} -> 
            io:format("from slave mode - recieved cambioEstado with NuevoEstado ~w~n", [NuevoEstado]),
            slaveMode(Peers, NuevoEstado, DataMaster);
        {backup, Data} -> 
            io:format("from slave mode - recieved backup with Data: ~w~n",[Data]),
            slaveMode(Peers, EstadoMaster, Data);
        masterDown ->
            io:format("from slave mode - masterDown with EstadoMaster=~w~n",[EstadoMaster]),
            case EstadoMaster of 
                esperarApuestas -> esperarApuestas(Peers, DataMaster);
                imprimirApuestas -> imprimirApuestas(Peers, DataMaster)
            end
    end.

startMaster(Peers) ->
    io:format("from master mode~n"),
    esperarApuestas(Peers, []).

esperarApuestas(Peers, Apuestas) ->
    io:format("esperando apuestas - Apuestas: ~w~n",[Apuestas]),
    receive
        {apostar, Cliente, Apuesta} -> 
            io:format("Apuesta de $~w con cliente ~w ~n",[Apuesta, Cliente]),
            ApuestasUpdated = [{Cliente, Apuesta} | Apuestas],
            backup(Peers, ApuestasUpdated),
            esperarApuestas(Peers, ApuestasUpdated)
        %1min
        after 60000 -> 
            io:format("Procesar apuestas"),
            imprimirApuestas(Peers, Apuestas)
    end.

imprimirApuestas(Peers, Apuestas) ->
    io:format("from imprimirApuestas con Apuestas: ~w~n",[Apuestas]),
    cambioEstado(Peers, imprimirApuestas),
    %Test when crash during this process (20sec)
    %timer:sleep(20000),
    lists:foreach(
        fun (Apuesta) ->
            {Cliente, ApuestaValue} = Apuesta,
            ApuestasPrinted = lists:delete(Apuesta, Apuestas),
            io:format("Apuesta $~w de cliente ~w~n",[ApuestaValue, Cliente]),
            backup(Peers, ApuestasPrinted),
            %10sec
            timer:sleep(10000)
        end, Apuestas),
    io:format("Apuestas impresas~n"),
    cambioEstado(Peers, esperarApuestas),
    esperarApuestas(Peers, []).

backup(Peers, Data) ->
    lists:foreach(fun (Peer) -> Peer ! {backup, Data} end, Peers).

cambioEstado(Peers, Estado) ->
    lists:foreach(fun (Peer) -> Peer ! {cambioEstado, Estado} end, Peers).