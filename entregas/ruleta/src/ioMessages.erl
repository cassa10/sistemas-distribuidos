-module(ioMessages).

-export([formatApuestas/1, mensajePrincipal/2, mensajeApuesta/0, mensajeSeleccionarCategoria/0, mensajeNumeroGanador/1,
    mensajeDarValorApuesta/0, mensajeDarValorPleno/0, mensajeGanancia/2, mensajeCargando/0, mensajeConfirmacion/0,
    errorValorApuesta/0, errorValorPleno/0, errorOpcionInvalida/0, errorOpcionInvalidaHome/0, mensajeFinDeRonda/0,
    mensajeGananciaTotal/1, mensajePerdida/2, mensajeIngresarUsuario/0, usuarioInvalido/0, errorSinApuestas/0]).

formatApuestas(Apuestas) ->
    integer_to_list(lists:foldl(fun(_, X) -> X + 1 end, 0, Apuestas)).

mensajeIngresarUsuario() ->
    "Por favor, ingrese un nombre de usuario: ".

usuarioInvalido() ->
    "Usuario invalido!".

mensajePrincipal(NombreUsuario, Apuestas) ->
    "\n# Usuario: " ++ NombreUsuario ++ "\n" ++
    "# Apuesta = {categoria/pleno, dinero apostado}\n" ++
    "# Nro Apuestas: " ++ formatApuestas(Apuestas) ++ "\n" ++
    io_lib:format("# Apuestas: ~w ~n~n",[Apuestas]) ++
    "Ingrese\n" ++
		"1. Apostar\n" ++
		"2. Deshacer apuestas\n" ++
		"3. Mandar apuestas\n" ++
		"\n0. Salir" ++
		"\nOpcion ingresada: ".

mensajeApuesta() ->
    "\nApostar a...\n" ++
    "1. Categoria\n" ++
    "2. Pleno\n" ++
    "\n0. Atras" ++
    "\nOpcion ingresada: ".

mensajeSeleccionarCategoria() ->
    "\nApostar a categoria...\n" ++
    "1. Rojo (1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)\n" ++
    "2. Negro (2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35)\n" ++
    "3. Par\n" ++
    "4. Impar\n" ++
    "5. Primera Mitad (1-18)\n" ++
    "6. Segunda Mitad (19-36)\n" ++
    "7. Primera Docena (1-12)\n" ++
    "8. Segunda Docena (13-24)\n" ++
    "9. Tercera Docena (25-36)\n" ++
    "10. Primera Columna (1,4,7,10,13,16,19,22,25,28,31,34)\n" ++
    "11. Segunda Columna (2,5,8,11,14,17,20,23,26,29,32,35)\n" ++
    "12. Tercera Columna (3,6,9,12,15,18,21,24,27,30,33,36)\n" ++
    "\n0. Atras" ++
    "\nOpcion ingresada: ".

mensajeDarValorPleno() ->
    "\nIngrese pleno: ".

mensajeDarValorApuesta() ->
    "\nIngrese su apuesta: ".

mensajeGanancia(CategoriaONumero, Valor) ->
    io_lib:format("~nApuesta a \"~w\" con ganancia de +$~w~n",[CategoriaONumero, Valor]).

mensajeGananciaTotal(Valor) ->
    io_lib:format("~nGanancia Total: +$~w~n",[Valor]).

mensajePerdida(CategoriaONumero, Valor) ->
    io_lib:format("~nApuesta a \"~w\" con perdida de -$~w~n",[CategoriaONumero, Valor]).

mensajeCargando() ->
    "Por favor, no se vaya y espere...\n".

mensajeConfirmacion() ->
    "Ingrese\n" ++
		"1. Ok\n" ++
		"Cualquier tecla. No\n" ++
		"Opcion ingresada: ".

errorValorApuesta() ->
    "No es un numero valido!\n".

errorValorPleno() ->
    "No es un numero pleno valido! Elija un numero entre 0 a 36!\n".

errorOpcionInvalida() ->
    "Opcion invalida.\n".

errorOpcionInvalidaHome() ->
    "Input invalido! Por favor, ingresa un valor entre 0 al 3" ++
    "\nOpcion ingresada: ".

errorSinApuestas() ->
    "No posee ninguna apuesta. Por favor, agregue al menos una apuesta y vuelva a intentarlo!"++
    "\nOpcion ingresada: ".

mensajeFinDeRonda() ->
    "|----------------------------------------------------------|\n" ++
    "|                     Fin de Ronda                         |\n" ++
    "|----------------------------------------------------------|\n".

mensajeNumeroGanador(NumeroGanador) ->
    io_lib:format("~n NUMERO GANADOR: ~w - CON CATEGORIAS: ~w ~n",[NumeroGanador, ruleta:numberCategoryMap(NumeroGanador)]).