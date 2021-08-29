-module(testServer).

-export([init/2, start/2, initByType/2, slaveMode/3, startMaster/1, esperarApuestas/2, imprimirApuestas/2, backup/2, cambioEstado/2]).


start(Id, IsMaster) ->
    spawn(fun() -> init(Id, IsMaster) end).


init(_, IsMaster) ->
    receive
        {peers, Peers} ->
            waitMaster(Peers);
        stop ->
            ok
    end.

waitMaster(IsMaster, Peers) ->
    receive
        master -> startMaster(Peers);
        slave  -> slaveMode(Peers, esperarApuestas, [])
    end.

slaveMode(Peers, EstadoMaster, DataMaster) ->
    receive
        {cambioEstado, NuevoEstado} -> slaveMode(Peers, NuevoEstado, DataMaster);
        {backup, Data} -> slaveMode(Peers, EstadoMaster, Data);
        masterDown -> 
            case EstadoMaster of 
                esperarApuestas -> esperarApuestas(Peers, DataMaster);
                imprimirApuestas -> imprimirApuestas(Peers, DataMaster)
            end
    end.

startMaster(Peers) ->
    esperarApuestas(Peers, []).

esperarApuestas(Peers, Apuestas) ->
    receive
        {apuesta, Apuesta} -> 
            ApuestasUpdated = [Apuesta | Apuestas],
            backup(Peers, ApuestasUpdated),
            esperarApuestas(Peers, ApuestasUpdated)
    end,
    imprimirApuestas(Peers, Apuestas).

imprimirApuestas(Peers, Apuestas) ->
    cambioEstado(Peers, imprimirApuestas),
    ApuestasPrinted = Apuestas,
    lists:foreach(
        fun (Apuesta) -> 
            io:format("Apuesta: "+Apuesta+"\n"),
            ApuestasPrinted = lists:delete(Apuesta, ApuestasPrinted),
            backup(Peers, ApuestasPrinted)
        end, Apuestas).

backup(Peers, Data) ->
    lists:foreach(fun (Peer) -> Peer ! {backup, Data} end, Peers).

cambioEstado(Peers, Estado) ->
    lists:foreach(fun (Peer) -> Peer ! {cambioEstado, Estado} end, Peers).