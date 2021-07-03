-module(usuario).

-export([start/1, conectar/1, apostar/4]).


start(NombreUsuario) ->
    ok.

conectar(Server) ->
    ok.
    %Server ! {conectar, self(), node()}.


apostar(Server, NombreUsuario, CategoriaONumero, Apuesta) ->
    %Validar CategoriaONumero

    %Server ! {NombreUsuario, self(), Apuesta, CategoriaONumero}
    ok.