-module(loadBalancer).

-export([start/2, init/1, waiting/2, handlingNodeDown/3, getSlaves/2, 
    upSlaves/1, upMaster/1, monitorNodes/1, selectMaster/1, deleteNodeDown/2]).

start(Id, Nodes) ->
    Pid = self(),
    logger:logf("Start Load Balancer with ~w id and ~w as pid.", [Id, Pid]),
    register(Id, Pid),
    init(Nodes).

init(Nodes) ->
    MasterNode = selectMaster(Nodes),
    upMaster(MasterNode),
    Slaves = getSlaves(MasterNode, Nodes),
    upSlaves(Slaves),
    monitorNodes(Nodes),
    waiting(MasterNode, Nodes).

waiting(MasterNode, Nodes) ->
    logger:logf("waiting with Master Node ~w", [MasterNode]),
    receive
        %Node is server string only (NO {pid, node()})
        {nodedown, Node} -> 
            logger:logf("Se recibio nodedown con Node: ~w", [Node]),
            handlingNodeDown(Node, MasterNode, Nodes);
        {apostar, Apuesta} -> 
            logger:logf("Se recibio apostar con Apuesta: ~w", [Apuesta]),
            MasterNode ! {apostar, Apuesta},
            waiting(MasterNode, Nodes)
    end.

handlingNodeDown(NodeDown, MasterNode, Nodes) ->
    logger:logf("Manejando Caida del Nodo ~w", [NodeDown]),
    NodesUp = deleteNodeDown(NodeDown, Nodes),
    case NodesUp of
        [] -> exit(self(), error_all_nodes_down);
        _ -> logger:logf("Nodos funcionando: ~w.",[NodesUp])
    end,
    {_, MasterNodeSv} = MasterNode,
    case NodeDown of
        MasterNodeSv ->
            NewMaster = selectMaster(NodesUp),
            logger:logf("Nuevo Nodo Maestro ~w", [NewMaster]),
            NewMaster ! masterDown,
            waiting(NewMaster, NodesUp);
        _ ->
            logger:log("Nodo Master sigue funcionando"),
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
    logger:logf("Delete node down: ~w from Nodes: ~w", [NodeDown, Nodes]),
    NewList = lists:keydelete(NodeDown, 2, Nodes),
    logger:logf("Nodes new list: ~w", [NewList]),
    NewList.
