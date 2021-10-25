-module(loadBalancer).

-export([start/3]).

start(Id, Nodes, Logger) ->
    Pid = self(),
    Time = time:zero(),
    logger:logf(Logger, Time, "Start Load Balancer with ~w id and ~w as pid.", [Id, Pid]),
    register(Id, Pid),
    init(Nodes, Logger, Time).

init(Nodes, Logger, Time) ->
    MasterNode = selectMaster(Nodes),
    upMaster(MasterNode, Logger, Time),
    Slaves = getSlaves(MasterNode, Nodes),
    upSlaves(Slaves, Logger, Time),
    monitorNodes(Nodes),
    waiting(MasterNode, Nodes, Logger, Time).

waiting(MasterNode, Nodes, Logger, Time) ->
    logger:logf(Logger, Time, "waiting with Master Node ~w", [MasterNode]),
    receive
        %Node is server string only (NO {pid, node()})
        {nodedown, Node} -> 
            N_Time = time:inc(Time),
            logger:logf(Logger, N_Time, "Se recibio nodedown con Node: ~w", [Node]),
            handlingNodeDown(Node, MasterNode, Nodes, Logger, N_Time);
        {apostar, Apuesta} -> 
            N_Time = time:inc(Time),
            logger:logf(Logger, N_Time, "Se recibio apostar con Apuesta: ~w", [Apuesta]),
            sendApuestaToNodes(Nodes, Apuesta),
            waiting(MasterNode, Nodes, Logger, N_Time)
    end.

sendApuestaToNodes(Nodes, Apuesta) ->
    lists:foreach(
        fun(Node) -> 
            Node ! {apostar, Apuesta} 
        end, Nodes).


handlingNodeDown(NodeDown, MasterNode, Nodes, Logger, Time) ->
    logger:logf(Logger, Time, "Manejando Caida del Nodo ~w", [NodeDown]),
    NodesUp = deleteNodeDown(NodeDown, Nodes, Logger, Time),
    case NodesUp of
        [] -> exit(self(), error_all_nodes_down);
        _ -> logger:logf(Logger, Time, "Nodos funcionando: ~w.",[NodesUp])
    end,
    {_, MasterNodeSv} = MasterNode,
    case NodeDown of
        MasterNodeSv ->
            NewMaster = selectMaster(NodesUp),
            logger:logf(Logger, Time, "Nuevo Nodo Maestro ~w", [NewMaster]),
            NewMaster ! {masterDown},
            waiting(NewMaster, NodesUp, Logger, Time);
        _ ->
            logger:log(Logger, Time, "Nodo Master sigue funcionando"),
            waiting(MasterNode, NodesUp, Logger, Time)
    end.

getSlaves(MasterNode, Nodes) -> 
    lists:delete(MasterNode, Nodes).

upSlaves(Slaves, Logger, Time) ->
    logger:logf(Logger, Time, "up Slaves Nodes ~w", [Slaves]),
    lists:foreach(fun(Slave) -> Slave ! {slave} end, Slaves).

upMaster(MasterNode, Logger, Time) -> 
    logger:logf(Logger, Time, "Up master node ~w", [MasterNode]),
    MasterNode ! {master}.

monitorNodes(Nodes) ->
    lists:foreach(fun({_, Node}) -> monitor_node(Node, true) end, Nodes).

% Head
selectMaster([]) -> error_node_empty_list;
selectMaster([Node | _ ]) -> Node.

deleteNodeDown(NodeDown, Nodes, Logger, Time) ->
    logger:logf(Logger, Time, "Delete node down: ~w from Nodes: ~w", [NodeDown, Nodes]),
    NewList = lists:keydelete(NodeDown, 2, Nodes),
    logger:logf(Logger, Time, "Nodes new list: ~w", [NewList]),
    NewList.
