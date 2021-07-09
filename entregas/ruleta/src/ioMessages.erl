-module(ioMessages).

-export([formatApuestas/1, mensajePrincipal/2, mensajePrincipal/2, mensajeApuesta/1, mensajeSeleccionarCategoria/0, mensajeDarValorApuesta/0]).

formatApuestas(Apuestas) ->
    Apuestas.

mensajePrincipal(NombreUsuario, Apuestas) ->
    "Usuario: " ++ NombreUsuario ++ "\n" ++
    "Apuestas actuales: " ++ formatApuestas(Apuestas) ++ "\n\n" ++
    "Selecciona\n" ++
		"1. Apostar" ++
		"2. Deshacer apuestas\n" ++
		"3. Mandar apuestas\n" ++
		"\n0. Salir\n" ++
		"Opcion ingresada: ".

mensajeApuesta(Apuestas) ->
    not_implemented.

mensajeSeleccionarCategoria() ->
    not_implemented.

mensajeDarValorApuesta() ->
    not_implemented.