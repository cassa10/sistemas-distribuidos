-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref()).

entry(Value, Time) ->
    receive
        {read, Ref, Handler} ->
            Handler ! {Ref, self(), Value, Time},
            entry(Value, Time);
        {check, Ref, Read, Handler} ->
            if
                Read == Time ->
                    Handler ! {Ref, ok};
                true ->
                    Handler ! {Ref, abort}
            end,
            entry(Value, Time);
        {write, New} ->
            entry(New, make_ref());
        stop ->
            ok
    end.
