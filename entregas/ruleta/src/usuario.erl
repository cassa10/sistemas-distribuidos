-module(usuario).

-export([start/2]).

start(Id, Server) ->
    register(Id, self()),
    initUsername(Id, Server, ioMessages:mensajeIngresarUsuario()).

salir() ->
    ok.

initUsername(Id, Server, InsertUsernameMessage) ->
    NombreUsuario = io:get_line(InsertUsernameMessage),
    case NombreUsuario of
        "" ->
            io:format(ioMessages:usuarioInvalido()),
            initUsername(Id, Server, InsertUsernameMessage);
        _ ->
            manejarInputUsuario(Server, Id, NombreUsuario, [], ioMessages:mensajePrincipal(NombreUsuario, []))
    end.

manejarInputUsuario(Server, Id, NombreUsuario, Apuestas, ReadMessage) ->
    %Generar Inputs de usuario (IO), donde:
    case io:read(ReadMessage) of
        % 1. Apostar
        {ok, 1} ->
            elegirApuesta(Server, Id, NombreUsuario, Apuestas);
        % 2. Borrar Apuestas
        {ok, 2} ->
            %Dar una confimacion (1. ok, cualquier otra cosa: rollback)
            borrarApuestas(Server, Id, NombreUsuario, Apuestas);
        % 3. Mandar Apuestas
        {ok, 3} ->
            case Apuestas of
                [] -> manejarInputUsuario(Server, Id, NombreUsuario, Apuestas, ioMessages:errorSinApuestas());
                _  -> mandarApuestas(Server, Id, NombreUsuario, Apuestas)
            end;
        % 0. Salir
        {ok, 0} -> salir();
        _       ->
            manejarInputUsuario(Server, Id, NombreUsuario, Apuestas, ioMessages:errorOpcionInvalidaHome())
    end.

elegirApuesta(Server, Id, NombreUsuario, Apuestas) ->
    %Manejar inputs apuesta: categoria o pleno(numero).
    case io:read(ioMessages:mensajeApuesta()) of
        % 1. Categorias
        {ok, 1} ->
            Categoria = obtenerApuestaPorCategoria(Server, Id, NombreUsuario, Apuestas),
            ValorApuesta = obtenerValorApuesta(Apuestas),
            Apuesta = {Categoria, ValorApuesta},
            ApuestasNew = agregarApuestaEnMisApuestas(Apuestas, Apuesta),
            manejarInputUsuario(Server, Id, NombreUsuario, ApuestasNew, ioMessages:mensajePrincipal(NombreUsuario, ApuestasNew));
        % 2. Pleno (Numero)
        {ok, 2} ->
            Pleno = obtenerApuestaPorPleno(),
            ValorApuesta = obtenerValorApuesta(Apuestas),
            Apuesta = {Pleno, ValorApuesta},
            ApuestasNew = agregarApuestaEnMisApuestas(Apuestas, Apuesta),
            manejarInputUsuario(Server, Id, NombreUsuario, ApuestasNew, ioMessages:mensajePrincipal(NombreUsuario, ApuestasNew));
        % 0. Atras
        {ok, 0} -> manejarInputUsuario(Server, Id, NombreUsuario, Apuestas, ioMessages:mensajePrincipal(NombreUsuario, Apuestas));
        _       ->
            io:format(ioMessages:errorOpcionInvalida()),
            elegirApuesta(Server, Id, NombreUsuario, Apuestas)
    end.

obtenerApuestaPorCategoria(Server, Id, NombreUsuario, Apuestas) ->
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
        {ok, 0} -> elegirApuesta(Server, Id, NombreUsuario, Apuestas);
        _       ->
            io:format(ioMessages:errorOpcionInvalida()),
            obtenerApuestaPorCategoria(Server, Id, NombreUsuario, Apuestas)
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

mandarApuestas(Server, Id, NombreUsuario, Apuestas) ->
    lists:foreach(
        fun (Apuesta) ->
            Server ! {apostar, {NombreUsuario, {Id, node()}, Apuesta}}
        end, Apuestas),
    ApuestaSize = sizeList(Apuestas),
    esperarFinRonda(Server, Id, NombreUsuario, ApuestaSize, 0, true).

chequearSiMostrarNumeroGanador(DebeMostrar, NumeroGanador) ->
    case DebeMostrar of
        true -> io:format(ioMessages:mensajeNumeroGanador(NumeroGanador));
        _ -> nothing_do
    end.

esperarFinRonda(Server, Id, NombreUsuario, ApuestaSize, GananciaTotal, IsFirstTime) ->
    io:format(ioMessages:mensajeCargando()),
    receive
        {ganancia, NumeroGanador, CategoriaONumero, Ganancia} ->
            chequearSiMostrarNumeroGanador(IsFirstTime, NumeroGanador),
            io:format(ioMessages:mensajeGanancia(CategoriaONumero, Ganancia)),
            GananciaTotalActualizado = GananciaTotal + Ganancia,
            esperarFinRondaSiCorresponde(Server, Id, NombreUsuario, ApuestaSize - 1, GananciaTotalActualizado, false);
        {perdida, NumeroGanador, CategoriaONumero, DineroApostado} ->
            chequearSiMostrarNumeroGanador(IsFirstTime, NumeroGanador),
            io:format(ioMessages:mensajePerdida(CategoriaONumero, DineroApostado)),
            esperarFinRondaSiCorresponde(Server, Id, NombreUsuario, ApuestaSize - 1, GananciaTotal, false)
        after 10000 ->
            esperarFinRonda(Server, Id, NombreUsuario, ApuestaSize, GananciaTotal, IsFirstTime)
    end.

esperarFinRondaSiCorresponde(Server, Id, NombreUsuario, ApuestasSize, GananciaTotal, IsFirstTime) ->
    if
        ApuestasSize > 0 -> esperarFinRonda(Server, Id, NombreUsuario, ApuestasSize, GananciaTotal, IsFirstTime);
        true -> 
            io:format(ioMessages:mensajeGananciaTotal(GananciaTotal)),
            io:format(ioMessages:mensajeFinDeRonda()),
            manejarInputUsuario(Server, Id, NombreUsuario, [], ioMessages:mensajePrincipal(NombreUsuario, []))
    end.

borrarApuestas(Server, Id, NombreUsuario, Apuestas) ->
    case io:read(ioMessages:mensajeConfirmacion()) of
        % 1. YES
        {ok, 1} -> manejarInputUsuario(Server, Id, NombreUsuario, [], ioMessages:mensajePrincipal(NombreUsuario, []));
        % cualquier otra cosa NO
        _ -> manejarInputUsuario(Server, Id, NombreUsuario, Apuestas, ioMessages:mensajePrincipal(NombreUsuario, Apuestas))
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
    { Categoria, Valor } = Apuesta,
    NuevaLista = lists:map(
	fun (ApuestaVieja) ->
	    { Categoria_, Valor_ } = ApuestaVieja,
	    case Categoria of
	      Categoria_ -> { Categoria, Valor_ + Valor };
	      _ -> ApuestaVieja
	    end
	end
    , Apuestas),
    case lists:keymember(Categoria, 1, NuevaLista) of
	false -> [Apuesta | Apuestas];
	true -> NuevaLista
    end.

sizeList([]) -> 0;
sizeList([_|XS]) -> 1 + sizeList(XS).
