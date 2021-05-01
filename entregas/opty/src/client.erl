-module(client).
-export([read/2, write/3, commit/1, abort/1]).

read(Handler, N) ->
    Ref = make_ref(),
    Handler ! {read, Ref, N},
    receive
        {Ref, Value} ->
            Value
    end.

write(Handler, N, Value) ->
    Handler ! {write, N, Value}.

commit(Handler) ->
    Ref = make_ref(),
    Handler ! {commit, Ref},
    receive
        {Ref, ok} -> 
            ok;
        {Ref, abort} -> 
            abort
    end.

abort(Handler) -> 
    Handler ! abort.