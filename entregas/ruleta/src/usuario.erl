-module(usuario).

-export([init/1, apostar/4]).

init(Servers, NombreUsuario) ->
    manejarInputUsuario(Servers, NombreUsuario, [], ioMessages:mensajePrincipal(NombreUsuario, Apuestas)).

salir() ->
    exit.

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
            Apuesta = {obtenerApuestaPorCategoria(), obtenerValorApuesta()},
            agregarApuestaEnMisApuestas(Apuestas, Apuesta),
            manejarInputUsuario();
        % 2. Pleno (Numero)
        {ok, 2} -> 
            Apuesta = obtenerApuestaPorPleno(),
            agregarApuestaEnMisApuestas(Apuestas, Apuesta),
            manejarInputUsuario();
        % 0. Atras
        {ok, 0} -> manejarInputUsuario(Servers, NombreUsuario, Apuestas)
    end.
    %Validar CategoriaONumero y Apuesta
    %Appendear la apuesta a lista de apuestas a realizar

    ok.

obtenerApuestaPorCategoria() ->
    %Manejar inputs de categoria.
    case io:read(ioMessages:mensajeSeleccionarCategoria(Apuestas)) of
        % 1. Rojo
        {ok, 1} -> rojo;
        % 2. Negro
        % {ok, 2} -> negro;
        % 3. Par
        % 4. Impar
        % ...
        % 0. Atras
    end.

obtenerValorApuesta() ->
    Valor = io:read(ioMessages:mensajeDarValorApuesta(Apuestas))
    %Validar valor (no negativo), en caso de que si volver a hacer el io:read?
    case validarValorApuesta(Valor) of
        true  -> Valor;
        false -> 
            %LOG ERROR y volver a tomar otro valor
            obtenerValorApuesta()
    end.

validarValorApuesta(Valor) ->
    case Valor of
        %TODO: Verificar si N lo toma como String o que??
        {ok, N} when list_to_integer(N) >= 0 -> true;
        _ -> false
    end.

mandarApuestas(Servers, Apuestas) ->
    %Server ! {NombreUsuario, self(), [{Apuesta, CategoriaONumero}, ...]}
    esperarFinRonda()

esperarFinRonda(Servers, NombreUsuario) ->
    receive 
        {ganancia, N} -> 
            %LOG GANANCIA 
            manejarInputUsuario(Servers, NombreUsuario, [], N)
        after 10
            %LOADING... en input
            esperarFinRonda()
    end.

