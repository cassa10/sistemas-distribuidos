-module(multicast).
-export([start/1]).

start(Jitter) ->
    spawn(fun() -> init(Jitter) end).

init(Jitter) ->
    receive
        {peers, Nodes} -> 
            open(Jitter, Nodes);
        stop ->
            ok
    end.

open(Jitter, Nodes) ->
    receive
        {start, Master} ->
            Cast = [],
            Queue = [],
            Next = 0,
            server(Master, Next, Nodes, Cast, Queue, Jitter);
        stop ->
            ok
    end.

server(Master, Next, Nodes, Cast, Queue, Jitter) ->
    receive
        {send, Msg} ->
            Ref = make_ref(),
            request(self(), Nodes, Ref, Msg),
            Cast2 = cast(Cast, Ref, length(Nodes) + 1),
            server(Master, Next, Nodes, Cast2, Queue, Jitter);
        {request, From, Ref, Msg} ->
            From ! {proposal, Ref, Next},
            Queue2 = insert(Queue, Ref, Msg, Next),
            Next2 = increment(Next),
            server(Master, Next2, Nodes, Cast, Queue2, Jitter);
        {proposal, Ref, Proposal} ->
            case proposal(Cast, Ref, Proposal) of
                {agreed, Seq, Cast2} ->
                    agree(Nodes, Ref, Seq),
                    server(Master, Next, Nodes, Cast2, Queue, Jitter);
                Cast2 ->
                    server(Master, Next, Nodes, Cast2, Queue, Jitter)
            end;
        {agreed, Ref, Seq} ->
            timer:sleep(Jitter),
            Max = lists:max(Seq),
            Updated = update(Queue, Ref, Max),
            {Agreed, Queue2} = agreed(Max, Updated),
            deliver(Master, Agreed),
            Next2 = increment(Max, Next),
            server(Master, Next2, Nodes, Cast, Queue2, Jitter)
    end.

increment(N) ->
    N + 1.

increment(M, N) ->
    increment(max(M, N)).

insert(Queue, Ref, Msg, Next) ->
    lists:keysort(3, [{proposal, Ref, Next, Msg} | Queue]).

request(From, Nodes, Ref, Msg) ->
    lists:foreach(fun(Node) ->
        Node ! {request, From, Ref, Msg}
    end, [From | Nodes]).

cast(Cast, Ref, Size) ->
    [{Ref, Size, []} | Cast].

agree(Nodes, Ref, Seq) ->
    lists:foreach(fun(Node) ->
        Node ! {agreed, Ref, Seq}
    end, [self() | Nodes]).

proposal(Cast, Ref, Proposal) ->
    case lists:keyfind(Ref, 1, Cast) of
        {Ref, 1, Seq} -> 
            {agreed, [Proposal | Seq], lists:keydelete(Ref, 1, Cast)};
        {Ref, N, Seq} -> 
            lists:keyreplace(Ref, 1, Cast, {Ref, N-1, [Proposal | Seq]});
        false -> 
            io:format("multicast: error proposal~n", [])
    end.

agreed(_, Queue) ->
    lists:splitwith(fun(X) ->
        case X of
            {agreed, _, _, _} -> true;
            _ -> false
        end
    end, Queue).

deliver(Master, Agreed) ->
    lists:foreach(fun({ _, _, _, Msg}) ->
        Master ! {response, Msg}
    end, Agreed).

update(Queue, Ref, Max) ->
    case lists:keyfind(Ref, 2, Queue) of
        {_, Ref, _, Msg} -> 
            lists:keysort(3, lists:keyreplace(Ref, 2, Queue, {agreed, Ref, Max, Msg}));
        false -> 
            io:format("multicast: error update~n",[])
    end.