-module(usuario).

-export([init/2, salir/0, manejarInputUsuario/4, elegirApuesta/3, obtenerValorApuesta/1, obtenerApuestaPorCategoria/3, validarValorApuesta/1, mandarApuestas/2, esperarFinRonda/2]).

init(Servers, NombreUsuario) ->
    manejarInputUsuario(Servers, NombreUsuario, [], ioMessages:mensajePrincipal(NombreUsuario, [])).

salir() ->
    exit.

borrarApuestas(Servers, NombreUsuario, Apuestas) -> ok.
mandarApuestas(Servers, NombreUsuario, Apuestas) -> ok.
manejarInputUsuario() -> ok.
obtenerApuestaPorPleno() -> ok.
agregarApuestaEnMisApuestas(A, B) -> ok.

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
        _       -> manejarInputUsuario(Servers, NombreUsuario, Apuestas, "Input invalido! Por favor, ingresa un valor entre 0 al 3")
    end.

elegirApuesta(Servers, NombreUsuario, Apuestas) ->
    %Manejar inputs apuesta: categoria o pleno(numero).
    case io:read(ioMessages:mensajeApuesta(Apuestas)) of
        % 1. Categorias
        {ok, 1} ->
            Apuesta = {obtenerApuestaPorCategoria(Servers, NombreUsuario, Apuestas), obtenerValorApuesta(Apuestas)},
            agregarApuestaEnMisApuestas(Apuestas, Apuesta),
            manejarInputUsuario();
        % 2. Pleno (Numero)
        {ok, 2} ->
            Apuesta = obtenerApuestaPorPleno(),
            agregarApuestaEnMisApuestas(Apuestas, Apuesta),
            manejarInputUsuario();
        % 0. Atras
        {ok, 0} -> manejarInputUsuario(Servers, NombreUsuario, Apuestas, "Volvio para atras")
    end,
    %Validar CategoriaONumero y Apuesta
    %Appendear la apuesta a lista de apuestas a realizar

    ok.

obtenerApuestaPorCategoria(Servers, NombreUsuario, Apuestas) ->
    %Manejar inputs de categoria.
    case io:read(ioMessages:mensajeSeleccionarCategoria(Apuestas)) of
        {ok, 1} -> rojo;
        {ok, 2} -> negro;
        {ok, 3} -> par;
        {ok, 4} -> impar;
        {ok, 5} -> primera_mitad;
        {ok, 6} -> segunda_mitad;
        {ok, 7} -> primera_docena;
        {ok, 8} -> segunda_docena;
        {ok, 0} -> elegirApuesta(Servers, NombreUsuario, Apuestas)
    end.

obtenerValorApuesta(Apuestas) ->
    Valor = io:read(ioMessages:mensajeDarValorApuesta(Apuestas)),
    %Validar valor (no negativo), en caso de que si volver a hacer el io:read?
    case validarValorApuesta(Valor) of
        true  -> Valor;
        false ->
            %LOG ERROR y volver a tomar otro valor
            obtenerValorApuesta(Apuestas)
    end.

validarValorApuesta(Valor) ->
    case Valor of
        %TODO: Verificar si N lo toma como String o que?? ver lo de list_to_integer
        {ok, N} when N >= 0 -> true;
        _ -> false
    end.

mandarApuestas(Servers, Apuestas) ->
    %Server ! {NombreUsuario, self(), [{Apuesta, CategoriaONumero}, ...]}
    esperarFinRonda(Servers, "Carlos__").

esperarFinRonda(Servers, NombreUsuario) ->
    receive
        {ganancia, N} ->
            %LOG GANANCIA
            manejarInputUsuario(Servers, NombreUsuario, [], N)
        after 10 ->
            %LOADING... en input
            esperarFinRonda(Servers, NombreUsuario)
    end.

