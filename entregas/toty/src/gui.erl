-module(gui).
-export([start/1, init/1]).

-include_lib("wx/include/wx.hrl").

start(Name) ->
    spawn(gui, init, [Name]).

init(Name) ->
    Width = 200,
    Height = 200,
    Server = wx:new(), %Server will be the parent for the Frame
    Frame = wxFrame:new(Server, -1, Name, [{size,{Width, Height}}]),
    wxFrame:show(Frame),
    loop(Frame).

loop(Frame)->
    receive
        start ->
            wxFrame:setBackgroundColour(Frame, {0, 0, 0}),
            wxFrame:refresh(Frame),
            loop(Frame);
        {draw, RGB} ->
            wxFrame:setBackgroundColour(Frame, RGB),
            wxFrame:refresh(Frame),
            loop(Frame);
        stop ->
            ok;
        Error ->
            io:format("gui: strange message ~w ~n", [Error]),
            loop(Frame)
    end.