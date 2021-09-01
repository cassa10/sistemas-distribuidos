-module(testLoadBalancer).

-export([start/2, init/2, handlingNodeDown/3, getSlaves/2, upSlaves/1, upMaster/1, monitorNodes/1, selectMaster/1]).

%Node = {ProccessRef, Node()}
% To send message to Node: "{ProccessRef, Node()} ! data"
start(Id, Nodes) ->
    register(Id, self()),
    io:format("Start Load Balancer with id "+ Id +"\n"),
    MasterNode = selectMaster(Nodes),
    upMaster(MasterNode),
    Slaves = getSlaves(MasterNode, Nodes),
    upSlaves(Slaves),
    monitorNodes(Nodes),
    init(MasterNode, Nodes).

%Cliente = {Id, Node()}
init(MasterNode, Nodes) ->
    timer:sleep(200),
    receive
        {nodedown, Node} -> handlingNodeDown(Node, MasterNode, Nodes);
        {apostar, Cliente, Apuesta} -> MasterNode ! {Cliente, Apuesta}
    end.

handlingNodeDown(NodeDown, MasterNode, Nodes) ->
    NodesUp = lists:delete(NodeDown, Nodes),
    case NodesUp of
        [] -> error_all_nodes_down
    end,
    case NodeDown of
        MasterNode ->
            NewMaster = selectMaster(NodesUp),
            NewMaster ! masterDown,
            init(NewMaster, NodesUp);
        _ -> init(MasterNode, NodesUp)
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