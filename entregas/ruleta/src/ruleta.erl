-module(ruleta).

-export([start/2, init/1, numberCategoryMap/1, esperarApuestas/3,
    empezarRonda/2, procesarApuestas/3, informarPerdida/2,
    esGanador/2, pagarApuesta/3, pagarNumero/1, pagarCategoria/2, girarRuleta/0]).

start(Id, Nodes) ->
    Pid = self(),
    logger:logf("Start server with ~w id and ~w as pid.~n", [Id, Pid]),
    register(Id, Pid),
    init(Nodes).

init(Nodes) ->
    waitLoadBalancer(Nodes).

waitLoadBalancer(Peers) ->
    logger:log("waiting load balancer...~n"),
    receive
        master -> 
            logger:log("received master~n"),
            masterMode(Peers);
        slave  -> 
            logger:log("received slave~n"),
            slaveMode(Peers, esperarApuestas, [], -1)
    end.

slaveMode(Peers, EstadoMaster, Apuestas, NumeroGanador) ->
    logger:log("En slave mode~n"),
    receive
        {cambioEstado, NuevoEstado} -> 
            logger:logf("slave mode - se recibio cambioEstado con NuevoEstado ~w~n",[NuevoEstado]),
            slaveMode(Peers, NuevoEstado, Apuestas, NumeroGanador);
        {replicarApuestas, ApuestasNuevas} ->
            logger:logf("slave mode - se recibio replicate apuestas con apuestas: ~w~n",[ApuestasNuevas]),
            slaveMode(Peers, EstadoMaster, ApuestasNuevas, NumeroGanador);
        {replicarNumeroGanador, ActualNumeroGanador} ->
            logger:logf("slave mode - se recibio replicate numero ganador con numero ganador: ~w~n",[ActualNumeroGanador]),
            slaveMode(Peers, EstadoMaster, Apuestas, ActualNumeroGanador);
        masterDown ->
            logger:logf("slave mode - se recibio masterDown con EstadoMaster: ~w , NumeroGanador: ~w ~n", [EstadoMaster, Apuestas, NumeroGanador]),
            case EstadoMaster of
                esperarApuestas -> esperarApuestas(Peers, Apuestas, 30000);
                empezarRonda -> empezarRonda(Peers, Apuestas);
                procesarApuestas -> procesarApuestas(Peers, NumeroGanador, Apuestas)
            end
    end.

masterMode(Peers) ->
    esperarApuestas(Peers, [], 30000).

esperarApuestas(Peers, ApuestasDeUsuarios, TiempoRestante) ->
    replicarNumeroGanador(Peers, -1),
    replicarCambioDeEstado(Peers, esperarApuestas),
    % Apuesta = { nombre_usuario, PID_usuario, Apuesta_usuario, Category || Numero }
    Start = erlang:system_time(millisecond),
    receive
        {apostar, Apuesta} ->
            ApuestasDeUsuariosUpdated = [Apuesta | ApuestasDeUsuarios],
            TiempoTranscurrido = minusTimeAbs(erlang:system_time(millisecond), Start),
            %Backup_slaves
            replicarApuestas(Peers, ApuestasDeUsuariosUpdated),
            esperarApuestas(Peers, ApuestasDeUsuariosUpdated, minusTimeAbs(TiempoRestante, TiempoTranscurrido))
        after TiempoRestante ->
            fin_espera
    end,
    case ApuestasDeUsuarios of
        [] -> esperarApuestas(Peers, ApuestasDeUsuarios, 30000);
        _  -> empezarRonda(Peers, ApuestasDeUsuarios)
    end.

empezarRonda(Peers, ApuestasDeUsuarios) ->
    replicarCambioDeEstado(Peers, empezarRonda),
    NumeroGanador = girarRuleta(),
    replicarNumeroGanador(Peers, NumeroGanador),
    procesarApuestas(Peers, NumeroGanador, ApuestasDeUsuarios).

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

% Apuesta = { nombre_usuario, {PID_ID, UserNode}, Apuesta_usuario, Category || {numero, Numero} }
procesarApuestas(Peers, NumeroGanador, Apuestas) ->
    replicarCambioDeEstado(Peers, procesarApuestas),
    lists:foreach(
        fun (Apuesta) ->
            {_, NodoUsuario, DineroApostado, CategoriaONumero} = Apuesta,
            ApuestasUpdated = lists:delete(Apuesta, Apuestas),
            case esGanador(NumeroGanador, CategoriaONumero) of
                true -> pagarApuesta(NodoUsuario, DineroApostado, CategoriaONumero);
                false -> informarPerdida(NodoUsuario, DineroApostado)
            end,
            %Replicar que la apuesta fue cobrada/pagada
            replicarApuestas(Peers, ApuestasUpdated)
        end, Apuestas),
    esperarApuestas(Peers, [], 30000).

esGanador(NumeroGanador, CategoriaONumeroApostado) ->
    case CategoriaONumeroApostado of
        {numero, N} -> NumeroGanador == N;
        _ -> lists:member(CategoriaONumeroApostado, numberCategoryMap(NumeroGanador))
    end.

pagarApuesta(NodoUsuario, DineroApostado, CategoriaONumero) ->
    case CategoriaONumero of
        {numero, _} ->
            NodoUsuario ! {ganancia, pagarNumero(DineroApostado)};
        _ ->
            NodoUsuario ! {ganancia, pagarCategoria(DineroApostado, CategoriaONumero)}
    end.

informarPerdida(NodoUsuario, DineroApostado) ->
    NodoUsuario ! {perdida, DineroApostado}.

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

replicarApuestas(Peers, Apuestas) ->
    sendPeers(Peers, {replicarApuestas, Apuestas}).

replicarNumeroGanador(Peers, NumeroGanador) ->
    sendPeers(Peers, {replicarNumeroGanador, NumeroGanador}).

replicarCambioDeEstado(Peers, Estado) ->
    sendPeers(Peers, {cambioEstado, Estado}).

sendPeers(Peers, Message) ->
    lists:foreach(fun (Peer) -> Peer ! Message end, Peers).