-module(testServer).

-export([log/1, logf/2, init/1, start/2, waitMaster/1, slaveMode/3, startMaster/1, esperarApuestas/2, imprimirApuestas/2, backup/2, cambioEstado/2]).

% Id :: atom
start(Id, Nodes) ->
    Pid = self(),
    logf("Start server with ~w id and ~w as pid.~n", [Id, Pid]),
    register(Id, Pid),
    init(Nodes).

init(Nodes) ->
    waitMaster(Nodes).

waitMaster(Peers) ->
    receive
        master -> 
            log("received master~n"),
            startMaster(Peers);
        slave  -> 
            log("received slave~n"),
            slaveMode(Peers, esperarApuestas, [])
    end.

slaveMode(Peers, EstadoMaster, DataMaster) ->
    log("from slave mode"),
    receive
        {cambioEstado, NuevoEstado} -> 
            logf("from slave mode - recieved cambioEstado with NuevoEstado ~w~n", [NuevoEstado]),
            slaveMode(Peers, NuevoEstado, DataMaster);
        {backup, Data} -> 
            logf("from slave mode - recieved backup with Data: ~w~n",[Data]),
            slaveMode(Peers, EstadoMaster, Data);
        masterDown ->
            logf("from slave mode - masterDown with EstadoMaster=~w~n",[EstadoMaster]),
            case EstadoMaster of 
                esperarApuestas -> esperarApuestas(Peers, DataMaster);
                imprimirApuestas -> imprimirApuestas(Peers, DataMaster)
            end
    end.

startMaster(Peers) ->
    log("from master mode~n"),
    esperarApuestas(Peers, []).

esperarApuestas(Peers, Apuestas) ->
    logf("esperando apuestas - Apuestas: ~w~n",[Apuestas]),
    receive
        {apostar, Cliente, Apuesta} -> 
            logf("Apuesta de $~w con cliente ~w ~n",[Apuesta, Cliente]),
            ApuestasUpdated = [{Cliente, Apuesta} | Apuestas],
            backup(Peers, ApuestasUpdated),
            esperarApuestas(Peers, ApuestasUpdated)
        %1min
        after 30000 -> 
            log("Procesar apuestas"),
            imprimirApuestas(Peers, Apuestas)
    end.

imprimirApuestas(Peers, Apuestas) ->
    logf("from imprimirApuestas con Apuestas: ~w~n",[Apuestas]),
    cambioEstado(Peers, imprimirApuestas),
    %Test when crash during this process (20sec)
    %timer:sleep(20000),
    lists:foreach(
        fun (Apuesta) ->
            {Cliente, ApuestaValue} = Apuesta,
            ApuestasPrinted = lists:delete(Apuesta, Apuestas),
            sendClient(Cliente, ApuestaValue, reward),
            logf("Apuesta $~w de cliente ~w~n",[ApuestaValue, Cliente]),
            backup(Peers, ApuestasPrinted),
            %10sec
            timer:sleep(10000)
        end, Apuestas),
    log("Apuestas impresas~n"),
    cambioEstado(Peers, esperarApuestas),
    esperarApuestas(Peers, []).

backup(Peers, Data) ->
    lists:foreach(fun (Peer) -> Peer ! {backup, Data} end, Peers).

cambioEstado(Peers, Estado) ->
    lists:foreach(fun (Peer) -> Peer ! {cambioEstado, Estado} end, Peers).

sendClient(Cliente, ApuestaValue, IsReward) ->
    Reward = ApuestaValue * 2,
    logf("Cliente: ~w, Reward: ~w, IsReward:~w ~n",[Cliente, Reward, IsReward]),
    Cliente ! {data, Reward},
    ok.

log(Message) -> 
    io:format("[~s] | ", [nowf()]),
    io:format(Message).

logf(Message, Params) -> 
        io:format("[~s] | ", [nowf()]),
    io:format(Message, Params).

%Return time now like "yyyy-mm-ddThh:mm:ss.zZ"
nowf() -> 
    Now = erlang:timestamp(),
    {_, _, MicroSecs} = Now,
    Ms = round(MicroSecs / 1000),
    {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_local_time(Now),
    Str = io_lib:format("~w-~w-~wT~w:~w:~w.~wZ",[Year, Month, Day, Hour, Minute, Second, Ms]),
    Str.