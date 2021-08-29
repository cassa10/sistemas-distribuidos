-module(testLoadBalancer).

-export([start/1, init/2, handlingNodeDown/3, getSlaves/2, upSlaves/1, upMaster/1, monitorNodes/1, selectMaster/1]).

start(Nodes) ->
    io:format("Start Load Balancer\n"),
    MasterNode = selectMaster(Nodes),
    upMaster(MasterNode),
    upSlaves(getSlaves(MasterNode, Nodes)),
    monitorNodes(Nodes),
    init(MasterNode, Nodes).

init(MasterNode, Nodes) ->
    timer:sleep(200),
    receive
        {nodedown, Node} -> handlingNodeDown(Node, MasterNode, Nodes);
        {apostar, Cliente, Apuesta} -> MasterNode ! {Cliente, Apuesta}
    end.

handlingNodeDown(NodeDown, MasterNode, Nodes) ->
    NodesUp = lists:delete(NodeDown, Nodes),
    case NodeDown of
        MasterNode ->
            NewMaster = selectMaster(NodesUp),
            NewMaster ! masterDown,
            init(NewMaster, Nodes);
        _          -> init(MasterNode, )
    end,

getSlaves(MasterNode, Nodes) ->
    lists:delete(ServerMaster, Servers).

upSlaves(Slaves) ->
    lists:foreach(fun(Slave) -> Slave ! slave end, Slaves).

upMaster(Master) -> 
    MasterNode ! master.

monitorNodes(Nodes) ->
    lists:foreach(fun(Node) -> monitor_node(Node, true) end, Nodes).

% Head
selectMaster([]) -> error_node_empty_list;
selectMaster([Node | T ]) -> Node.
