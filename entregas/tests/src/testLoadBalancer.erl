-module(testLoadBalancer).

-export([start/2, checkMaster/2, checkMasterStatus/1]).

start(ServerMaster, Servers) ->
    io:format("Load Balancer\n"),
    
    checkMaster(ServerMaster, Servers),
    receive
        {apostar, Cliente, Apuesta} -> ServerMaster ! {Cliente, Apuesta}
        after 60 ->
           start(ServerMaster, Servers)
    end.


checkMaster(ServerMaster, Servers) ->
    case checkMasterStatus(ServerMaster) of
        false -> handlingMasterDown(ServerMaster, Servers)
    end.

handlingMasterDown(ServerMaster, Servers) ->
    SlaveServers = lists:delete(ServerMaster, Servers),
    NewMaster = selectNewMaster(SlaveServers),
    NewMaster ! {masterDown},
    timer:sleep(500),
    start(NewMaster, Servers).

selectNewMaster([Slave|_]) ->
    Slave.

checkMasterStatus(ServerMaster) ->
    is_process_alive(ServerMaster).

