-module(usuario).

-export([init/2, salir/0, manejarInputUsuario/4, elegirApuesta/3, obtenerValorApuesta/1,
    obtenerApuestaPorCategoria/3, mandarApuestas/3, esperarFinRonda/2]).

init(Servers, NombreUsuario) ->
    manejarInputUsuario(Servers, NombreUsuario, [], ioMessages:mensajePrincipal(NombreUsuario, [])).

salir() ->
    ok.

manejarInputUsuario(Servers, NombreUsuario, Apuestas, ReadMessage) ->
    %Generar Inputs de usuario (IO), donde:
    case io:read(ReadMessage) of
        % 1. Apostar
        {ok, 1} ->
            elegirApuesta(Servers, NombreUsuario, Apuestas);
        % 2. Borrar Apuestas
        {ok, 2} ->
            %Dar una confimacion (1. ok, cualquier otra cosa: rollback)
            borrarApuestas(Servers, NombreUsuario, Apuestas);
        % 3. Mandar Apuestas
        {ok, 3} ->
            mandarApuestas(Servers, NombreUsuario, Apuestas);
        % 0. Salir
        {ok, 0} -> salir();
        _       ->
            manejarInputUsuario(Servers, NombreUsuario, Apuestas, ioMessages:errorOpcionInvalidaHome())
    end.

elegirApuesta(Servers, NombreUsuario, Apuestas) ->
    %Manejar inputs apuesta: categoria o pleno(numero).
    case io:read(ioMessages:mensajeApuesta()) of
        % 1. Categorias
        {ok, 1} ->
            Categoria = obtenerApuestaPorCategoria(Servers, NombreUsuario, Apuestas),
            ValorApuesta = obtenerValorApuesta(Apuestas),
            Apuesta = {Categoria, ValorApuesta},
            ApuestasNew = agregarApuestaEnMisApuestas(Apuestas, Apuesta),
            manejarInputUsuario(Servers, NombreUsuario, ApuestasNew, ioMessages:mensajePrincipal(NombreUsuario, ApuestasNew));
        % 2. Pleno (Numero)
        {ok, 2} ->
            Pleno = obtenerApuestaPorPleno(),
            ValorApuesta = obtenerValorApuesta(Apuestas),
            Apuesta = {Pleno, ValorApuesta},
            ApuestasNew = agregarApuestaEnMisApuestas(Apuestas, Apuesta),
            manejarInputUsuario(Servers, NombreUsuario, ApuestasNew, ioMessages:mensajePrincipal(NombreUsuario, ApuestasNew));
        % 0. Atras
        {ok, 0} -> manejarInputUsuario(Servers, NombreUsuario, Apuestas, ioMessages:mensajePrincipal(NombreUsuario, Apuestas));
        _       ->
            io:format(ioMessages:errorOpcionInvalida()),
            elegirApuesta(Servers, NombreUsuario, Apuestas)
    end.

obtenerApuestaPorCategoria(Servers, NombreUsuario, Apuestas) ->
    %Manejar inputs de categoria.
    case io:read(ioMessages:mensajeSeleccionarCategoria()) of
        {ok, 1} -> rojo;
        {ok, 2} -> negro;
        {ok, 3} -> par;
        {ok, 4} -> impar;
        {ok, 5} -> primera_mitad;
        {ok, 6} -> segunda_mitad;
        {ok, 7} -> primera_docena;
        {ok, 8} -> segunda_docena;
        {ok, 9} -> tercera_docena;
        {ok, 10} -> primera_columna;
        {ok, 11} -> segunda_columna;
        {ok, 12} -> tercera_columna;
        %Atras
        {ok, 0} -> elegirApuesta(Servers, NombreUsuario, Apuestas);
        _       ->
            io:format(ioMessages:errorOpcionInvalida()),
            obtenerApuestaPorCategoria(Servers, NombreUsuario, Apuestas)
    end.

obtenerValorApuesta(Apuestas) ->
    Input = io:get_line(ioMessages:mensajeDarValorApuesta()),
    case string:to_integer(Input) of
        {error, _} ->
            io:format(ioMessages:errorValorApuesta()),
            obtenerValorApuesta(Apuestas);
        {Value, _} when Value > 0 -> Value;
        _ ->
            io:format(ioMessages:errorValorApuesta()),
            obtenerValorApuesta(Apuestas)
    end.

mandarApuestas(Servers, NombreUsuario, Apuestas) ->
    %Server = Get_Server(Servers),
    %Server ! {NombreUsuario, self(), Apuestas},
    %LOGEAR APUESTAS QUE SE VAN A ENVIAR.
    esperarFinRonda(Servers, NombreUsuario).

esperarFinRonda(Servers, NombreUsuario) ->
    io:format(ioMessages:mensajeCargando()),
    receive
        {ganancia, N} ->
            %LOG GANANCIA
            io:format(ioMessages:mensajeGanancia(N)),
            manejarInputUsuario(Servers, NombreUsuario, [], ioMessages:mensajePrincipal(NombreUsuario, []))
        after 10000 ->
            esperarFinRonda(Servers, NombreUsuario)
    end.

borrarApuestas(Servers, NombreUsuario, Apuestas) ->
    case io:read(ioMessages:mensajeConfirmacion()) of
        % 1. YES
        {ok, 1} -> manejarInputUsuario(Servers, NombreUsuario, [], ioMessages:mensajePrincipal(NombreUsuario, []));
        % cualquier otra cosa NO
        _ -> manejarInputUsuario(Servers, NombreUsuario, Apuestas, ioMessages:mensajePrincipal(NombreUsuario, Apuestas))
    end.

obtenerApuestaPorPleno() ->
    Input = io:get_line(ioMessages:mensajeDarValorPleno()),
    case string:to_integer(Input) of
        {error, _} ->
            io:format(ioMessages:errorValorPleno()),
            obtenerApuestaPorPleno();
        {Value, _} when Value >= 0, Value =< 36 -> Value;
        _ ->
            io:format(ioMessages:errorValorPleno()),
            obtenerApuestaPorPleno()
    end.

agregarApuestaEnMisApuestas(Apuestas, Apuesta) ->
    [Apuesta | Apuestas].
