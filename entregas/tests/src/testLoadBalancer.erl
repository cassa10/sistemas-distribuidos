-module(testLoadBalancer).

-export([start/2, init/1, waiting/2, handlingNodeDown/3, getSlaves/2, 
    upSlaves/1, upMaster/1, monitorNodes/1, selectMaster/1, deleteNodeDown/2]).

%Node = {ProccessRef, Node()}
% To send message to Node: "{ProccessRef, Node()} ! data"
start(Id, Nodes) ->
    Pid = self(),
    io:format("Start Load Balancer with ~w id and ~w as pid.~n", [Id, Pid]),
    register(Id, Pid),
    init(Nodes).

init(Nodes) ->
    MasterNode = selectMaster(Nodes),
    upMaster(MasterNode),
    Slaves = getSlaves(MasterNode, Nodes),
    upSlaves(Slaves),
    monitorNodes(Nodes),
    waiting(MasterNode, Nodes)
.
%Cliente = {Id, Node()}
waiting(MasterNode, Nodes) ->
    io:format("waiting with Master Node ~w~n", [MasterNode]),
    timer:sleep(200),
    receive
        {nodedown, Node} -> 
            io:format("Received nodedown with Node: ~w~n", [Node]),
            handlingNodeDown(Node, MasterNode, Nodes);
        {apostar, Cliente, Apuesta} -> 
            io:format("Received apostar with Cliente: ~w , Apuesta: ~w ~n", [Cliente, Apuesta]),
            MasterNode ! {Cliente, Apuesta}
    end.

handlingNodeDown(NodeDown, MasterNode, Nodes) ->
    io:format("Handling Node Down ~w~n", [NodeDown]),
    NodesUp = deleteNodeDown(NodeDown, Nodes),
    case NodesUp of
        [] -> exit(self(), error_all_nodes_down);
        _ -> io:format("Nodes up: ~w.~n",[NodesUp])
    end,
    {_, MasterNodeSv} = MasterNode,
    case NodeDown of
        MasterNodeSv ->
            NewMaster = selectMaster(NodesUp),
            io:format("New Master Node ~w~n", [NewMaster]),
            NewMaster ! masterDown,
            waiting(NewMaster, NodesUp);
        _ ->
            io:format("MasterNode still up~n"),
            waiting(MasterNode, NodesUp)
    end.

getSlaves(MasterNode, Nodes) -> 
    lists:delete(MasterNode, Nodes).

upSlaves(Slaves) ->
    lists:foreach(fun(Slave) -> Slave ! slave end, Slaves).

upMaster(MasterNode) -> 
    MasterNode ! master.

monitorNodes(Nodes) ->
    lists:foreach(fun({_, Node}) -> monitor_node(Node, true) end, Nodes).

% Head
selectMaster([]) -> error_node_empty_list;
selectMaster([Node | _ ]) -> Node.

deleteNodeDown(NodeDown, Nodes) ->
    io:format("~nDelete node down: ~w from Nodes: ~w ~n", [NodeDown, Nodes]),
    NewList = lists:keydelete(NodeDown, 2, Nodes),
    io:format("Nodes new list: ~w ~n", [NewList]),
    NewList.
