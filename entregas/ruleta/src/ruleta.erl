-module(ruleta).

-export([start/0, girarRuleta/0, numberCategoryMap/1, payCategory/2]).

slaveMode(Peers) ->
    receive
        %{backup, ...data} ->  ..., slaveMode(Peers);
        %{masterDown, } -> ..., esperarApuestas([], [], 120);
        %...
    end

init(IsMaster, Peers) ->
    %Ver como hacer cuando hay slaves y que vayan guardando todos los datos a partir del envio de mensajes.
    case IsMaster of
        true -> esperarApuestas(Peers, [], [], 120);
        false -> slaveMode(Peers)
    end.

esperarApuestas(Peers, UsuariosConectados, ApuestasDeUsuarios, TiempoRestante) ->

    % La ruleta tiene una lista de usuarios conectados (limitados a X usuarios) y una lista de apuestas.
    % En cada tirada se espera 2 minutos:
    %   1) Si no hay nadie, vuelve a esperar;
    %   2) Si hay minimo un usuario conectado, empieza a ruletear.
    % Apuesta = { nombre_usuario, PID_usuario, Apuesta_usuario, Category || Numero }
    Start = erlang:system_time(seconds)

    %Chequear si algun usuario conectado esta muerto?

    %Hacer if si TiempoRestante es 0 o menor, saltear esto y ir directo al case ApuestasDeUsuarios...
    receive
        {conectar, NodoUsuario} -> 
            UsuariosConectadosUpdated = [NodoUsuario | UsuariosConectados],
            TiempoTranscurrido = erlang:system_time(seconds) - Start,
            %Backup_slaves
            esperarApuestas(UsuariosConectadosUpdated, ApuestasDeUsuarios, TiempoRestante - TiempoTranscurrido);
        {apostar, Apuesta} ->
            %evaluar si el usuarios pertenece a los que estan conectados...
            ApuestasDeUsuariosUpdated = [Apuesta | ApuestasDeUsuarios],
            TiempoTranscurrido = erlang:system_time(seconds) - Start,
            %Backup_slaves
            esperarApuestas(UsuariosConectados, ApuestasDeUsuariosUpdated, TiempoRestante - TiempoTranscurrido);
        after TiempoRestante ->
            fin_espera
            %Backup_slaves?
    end,
    case ApuestasDeUsuarios of
        [] -> esperarApuestas(UsuariosConectados, ApuestasDeUsuarios, 120);
        _  -> empezarRonda(UsuariosConectados, ApuestasDeUsuarios)
    end.

empezarRonda(UsuariosConectados, ApuestasDeUsuarios) ->
    NumeroGanador = girarRuleta(),
    %Ver de hacer un backup en los slaves, por cada apuesta pagada y luego filtrarlos.
    cobrarOPagarApuestas(NumeroGanador, ApuestasDeUsuarios),
    esperarApuestas(UsuariosConectados, [], 120).

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

% Apuesta = { nombre_usuario, PID_usuario, Apuesta_usuario, Category || {numero, Numero} }
cobrarOPagarApuestas(NumeroGanador, Apuestas) ->
    lists:foreach(
        fun ({ _, NodoUsuario, DineroApostado, CategoriaONumero}) ->  
            case esGanador(CategoriaONumero) of
                true -> pagarApuesta(NodoUsuario, DineroApostado, CategoriaONumero);
                false -> informarPerdida(NodoUsuario, DineroApostado)
            end
            %BACKUP, no volver a pagar las apuestas pagas, cuando levante el otro nodo si este se cae.
        end
    ).

informarPerdida(NodoUsuario, DineroApostado) ->
    NodoUsuario ! {perdida, DineroApostado}.

esGanador(NumeroGanador, CategoriaONumeroApostado) ->
    case CategoriaONumeroApostado of
        {numero, N} -> NumeroGanador == N;
        _ -> lists:member(CategoriaONumeroApostado, numberCategoryMap(NumeroGanador))
    end.

pagarApuesta(NodoUsuario, DineroApostado, CategoriaONumero) ->
    case CategoriaONumero of
        {numero, _} ->  
            Recompenza = pagarNumero(DineroApostado);
        _ -> 
            Recompenza = pagarCategoria(DineroApostado, CategoriaONumero),
    end,
    NodoUsuario ! {ganancia, Recompenza}.

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