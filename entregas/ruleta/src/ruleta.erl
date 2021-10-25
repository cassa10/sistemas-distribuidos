-module(ruleta).

-export([start/3]).

start(Id, Nodes, Logger) ->
    Pid = self(),
    Time = time:zero(),
    logger:logf(Logger, Time, "Start server with id: ~w, pid: ~w", [Id, Pid]),
    register(Id, Pid),
    init(Nodes, Logger, Time).

init(Nodes, Logger, Time) ->
    waitLoadBalancer(Nodes, Logger, Time).

waitLoadBalancer(Peers, Logger, Time) ->
    logger:log(Logger, Time, "waiting load balancer..."),
    receive
        {master} -> 
            N_Time = time:inc(Time),
            logger:log(Logger, N_Time, "received master"),
            masterMode(Logger, N_Time, Peers);
        {slave}  ->
            N_Time = time:inc(Time), 
            logger:log(Logger, N_Time, "received slave"),
            slaveMode(Logger, N_Time, Peers, esperarApuestas, [], -1)
    end.

slaveMode(Logger, Time, Peers, EstadoMaster, Apuestas, NumeroGanador) ->
    logger:log(Logger, Time, "En slave mode"),
    receive
        {terminoProcesarApuestas, NodeTime } ->
            N_Time = time:merge(Time, NodeTime), 
            logger:log(Logger, N_Time, "slave mode - se recibio terminoProcesarApuestas"),
            slaveMode(Logger, N_Time, Peers, esperarApuestas, [], -1);
        {cambioEstado, NodeTime, NuevoEstado} -> 
            N_Time = time:merge(Time, NodeTime),  
            logger:logf(Logger, N_Time, "slave mode - se recibio cambioEstado con NuevoEstado ~w",[NuevoEstado]),
            slaveMode(Logger, N_Time, Peers, NuevoEstado, Apuestas, NumeroGanador);
        {replicarApuestas, NodeTime, ApuestasNuevas} ->
            N_Time = time:merge(Time, NodeTime), 
            logger:logf(Logger, N_Time, "slave mode - se recibio replicate apuestas con apuestas: ~w",[ApuestasNuevas]),
            slaveMode(Logger, N_Time, Peers, EstadoMaster, ApuestasNuevas, NumeroGanador);
        {replicarNumeroGanador, NodeTime, ActualNumeroGanador} ->
            N_Time = time:merge(Time, NodeTime), 
            logger:logf(Logger, N_Time, "slave mode - se recibio replicate numero ganador con numero ganador: ~w",[ActualNumeroGanador]),
            slaveMode(Logger, N_Time, Peers, EstadoMaster, Apuestas, ActualNumeroGanador);
        {apuestaProcesada, NodeTime, Apuesta} ->
            N_Time = time:merge(Time, NodeTime),  
            logger:logf(Logger, N_Time, "slave mode - se recibio replicate apuesta procesada con apuesta: ~w",[Apuesta]),
            ApuestasUpdated = lists:delete(Apuesta, Apuestas),
            logger:logf(Logger, N_Time, "slave mode - actualizando apuestas replicadas: ~w",[ApuestasUpdated]),
            slaveMode(Logger, N_Time, Peers, EstadoMaster, ApuestasUpdated, NumeroGanador);
        {masterDown} ->
            N_Time = time:inc(Time), 
            logger:logf(Logger, N_Time, "slave mode - se recibio masterDown con EstadoMaster: ~w , Apuestas ~w, NumeroGanador: ~w", [EstadoMaster, Apuestas, NumeroGanador]),
            case EstadoMaster of
                esperarApuestas -> esperarApuestas(Logger, N_Time, Peers, Apuestas, 30000);
                empezarRonda -> empezarRonda(Logger, N_Time, Peers, Apuestas);
                procesarApuestas -> procesarApuestas(Logger, N_Time, Peers, NumeroGanador, Apuestas)
            end;
        {updateTime, NodeTime} ->
            N_Time = time:merge(Time, NodeTime),  
            slaveMode(Logger, N_Time, Peers, EstadoMaster, Apuestas, NumeroGanador)
    end.

masterMode(Logger, Time, Peers) ->
    esperarApuestas(Logger, Time, Peers, [], 30000).

esperarApuestas(Logger, Time, Peers, ApuestasDeUsuarios, TiempoRestante) ->
    logger:logf(Logger, Time, "Esperando apuestas con Apuestas de usuarios: ~w , Tiempo restante: ~w",[ApuestasDeUsuarios, TiempoRestante]),
    % Apuesta = { nombre_usuario, {PID, Node_Usuario}, Apuesta_usuario, Category || Numero }
    Start = erlang:system_time(millisecond),
    receive
        {apostar, Apuesta} ->
            N_Time = time:inc(Time),
            ApuestasDeUsuariosUpdated = [Apuesta | ApuestasDeUsuarios],
            TiempoTranscurrido = minusTimeAbs(erlang:system_time(millisecond), Start),
            %Backup_slaves
            replicarApuestas(Logger, N_Time, Peers, ApuestasDeUsuariosUpdated),
            esperarApuestas(Logger, N_Time, Peers, ApuestasDeUsuariosUpdated, minusTimeAbs(TiempoRestante, TiempoTranscurrido))
        after TiempoRestante ->
            fin_espera
    end,
    New_Time = time:inc(Time),
    updateTimePeers(Peers, New_Time),
    case ApuestasDeUsuarios of
        [] -> esperarApuestas(Logger, New_Time, Peers, ApuestasDeUsuarios, 30000);
        _  -> empezarRonda(Logger, New_Time, Peers, ApuestasDeUsuarios)
    end.

empezarRonda(Logger, Time, Peers, ApuestasDeUsuarios) ->
    replicarCambioDeEstado(Logger, Time, Peers, empezarRonda),
    NumeroGanador = girarRuleta(),
    replicarNumeroGanador(Logger, Time, Peers, NumeroGanador),
    procesarApuestas(Logger, Time, Peers, NumeroGanador, ApuestasDeUsuarios).

numberCategoryMap(N) ->
    case N of
        0  -> [par];
        1  -> [rojo, impar, primera_docena, primera_columna, primera_mitad];
        2  -> [negro, par, primera_docena, segunda_columna, primera_mitad];
        3  -> [rojo, impar, primera_docena, tercera_columna, primera_mitad];
        4  -> [negro, par, primera_docena, primera_columna, primera_mitad];
        5  -> [rojo, impar, primera_docena, segunda_columna, primera_mitad];
        6  -> [negro, par, primera_docena, tercera_columna, primera_mitad];
        7  -> [rojo, impar, primera_docena, primera_columna, primera_mitad];
        8  -> [negro, par, primera_docena, segunda_columna, primera_mitad];
        9  -> [rojo, impar, primera_docena, tercera_columna, primera_mitad];
        10 -> [negro, par, primera_docena, primera_columna, primera_mitad];
        11 -> [negro, impar, primera_docena, segunda_columna, primera_mitad];
        12 -> [rojo, par, primera_docena, tercera_columna, primera_mitad];
        13 -> [negro, impar, segunda_docena, primera_columna, primera_mitad];
        14 -> [rojo, par, segunda_docena, segunda_columna, primera_mitad];
        15 -> [negro, impar, segunda_docena, tercera_columna, primera_mitad];
        16 -> [rojo, par, segunda_docena, primera_columna, primera_mitad];
        17 -> [negro, impar, segunda_docena, segunda_columna, primera_mitad];
        18 -> [rojo, par, segunda_docena, tercera_columna, primera_mitad];
        19 -> [rojo, impar, segunda_docena, primera_columna, segunda_mitad];
        20 -> [negro, par, segunda_docena, segunda_columna, segunda_mitad];
        21 -> [rojo, impar, segunda_docena, tercera_columna, segunda_mitad];
        22 -> [negro, par, segunda_docena, primera_columna, segunda_mitad];
        23 -> [rojo, impar, segunda_docena, segunda_columna, segunda_mitad];
        24 -> [negro, par, segunda_docena, tercera_columna, segunda_mitad];
        25 -> [rojo, impar, tercera_docena, primera_columna, segunda_mitad];
        26 -> [negro, par, tercera_docena, segunda_columna, segunda_mitad];
        27 -> [rojo, impar, tercera_docena, tercera_columna, segunda_mitad];
        28 -> [negro, par, tercera_docena, primera_columna, segunda_mitad];
        29 -> [negro, impar, tercera_docena, segunda_columna, segunda_mitad];
        30 -> [rojo, par, tercera_docena, tercera_columna, segunda_mitad];
        31 -> [negro, impar, tercera_docena, primera_columna, segunda_mitad];
        32 -> [rojo, par, tercera_docena, segunda_columna, segunda_mitad];
        33 -> [negro, impar, tercera_docena, tercera_columna, segunda_mitad];
        34 -> [rojo, par, tercera_docena, primera_columna, segunda_mitad];
        35 -> [negro, impar, tercera_docena, segunda_columna, segunda_mitad];
        36 -> [rojo, par, tercera_docena, tercera_columna, segunda_mitad]
    end.

% Apuesta = { nombre_usuario, {PID_ID, UserNode}, {Categoria || Integer, DineroApostado}}
procesarApuestas(Logger, Time, Peers, NumeroGanador, Apuestas) ->
    replicarCambioDeEstado(Logger, Time, Peers, procesarApuestas),
    logger:logf(Logger, Time, "Procesando apuestas: ~w con numero ganador: ~w y categorias ganadoras: ~w",[Apuestas, NumeroGanador, numberCategoryMap(NumeroGanador)]),
    lists:foreach(
        fun (Apuesta) ->
            {_, NodoUsuario, {CategoriaONumero, DineroApostado}} = Apuesta,
            logger:logf(Logger, Time, "ProcesandoApuestas - apuesta: ~w", [Apuesta]),
            %TODO: Sleep de prueba
            timer:sleep(10000),
            case esGanador(NumeroGanador, CategoriaONumero) of
                true -> pagarApuesta(NodoUsuario, DineroApostado, CategoriaONumero);
                false -> informarPerdida(NodoUsuario, DineroApostado, CategoriaONumero)
            end,
            %Replicar que la apuesta fue cobrada/pagada
            replicarApuestasProcesada(Logger, Time, Peers, Apuesta)
        end, Apuestas),
    resetReplicaNumeroGanadorEIrAEsperarApuestas(Logger, Time, Peers).

resetReplicaNumeroGanadorEIrAEsperarApuestas(Logger, Time, Peers) ->
    replicarResetEsperarApuestas(Logger, Time, Peers),
    esperarApuestas(Logger, Time, Peers, [], 30000).

esGanador(NumeroGanador, CategoriaONumeroApostado) ->
    case is_integer(CategoriaONumeroApostado) of
        true -> NumeroGanador == CategoriaONumeroApostado;
        false -> lists:member(CategoriaONumeroApostado, numberCategoryMap(NumeroGanador))
    end.

pagarApuesta(NodoUsuario, DineroApostado, CategoriaONumero) ->
    case CategoriaONumero of
        {numero, _} ->
            NodoUsuario ! {ganancia, CategoriaONumero, pagarNumero(DineroApostado)};
        _ ->
            NodoUsuario ! {ganancia, CategoriaONumero, pagarCategoria(DineroApostado, CategoriaONumero)}
    end.

informarPerdida(NodoUsuario, DineroApostado, CategoriaONumero) ->
    NodoUsuario ! {perdida, CategoriaONumero, DineroApostado}.

pagarNumero(Apuesta) ->
    Apuesta * 36.

pagarCategoria(Apuesta, Categoria) ->
    EsDoble = lists:member(Categoria, [par,impar, rojo, negro, primera_mitad, segunda_docena]),
    case EsDoble of
        true -> Apuesta * 2;
        false -> Apuesta * 3
    end.

girarRuleta() ->
    rand:uniform(37) - 1.

minusTimeAbs(Time1,Time2) ->
    DifTime = Time1 - Time2,
    if 
        DifTime >= 0 -> DifTime;
        true -> 0
    end.

replicarApuestas(Logger, Time, Peers, Apuestas) ->
    logger:logf(Logger, Time, "Replicando apuestas~w", [Apuestas]),
    sendPeers(Peers, {replicarApuestas, Time, Apuestas}).

replicarNumeroGanador(Logger, Time, Peers, NumeroGanador) ->
    logger:logf(Logger, Time, "Replicando numero ganador ~w", [NumeroGanador]),
    sendPeers(Peers, {replicarNumeroGanador, Time, NumeroGanador}).

replicarCambioDeEstado(Logger, Time, Peers, Estado) ->
    logger:logf(Logger, Time, "Replicando cambio de estado a ~w", [Estado]),
    sendPeers(Peers, {cambioEstado, Time, Estado}).

replicarResetEsperarApuestas(Logger, Time, Peers) ->
    logger:log(Logger, Time, "Replicando reset esperar apuestas"),
    sendPeers(Peers, {terminoProcesarApuestas, Time}).

replicarApuestasProcesada(Logger, Time, Peers, Apuesta) ->
    logger:logf(Logger, Time, "Replicando eliminar apuesta procesada - apuesta: ~w",[Apuesta]),
    sendPeers(Peers, {apuestaProcesada, Time, Apuesta}).

updateTimePeers(Peers, Time) ->
    sendPeers(Peers, {updateTime, Time}).

sendPeers(Peers, Message) ->
    lists:foreach(fun (Peer) -> Peer ! Message end, Peers).