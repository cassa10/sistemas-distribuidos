Ruleta
=====

An OTP library

Build
-----

    $ rebar3 compile


Script
-------

1) Instance Loggy (Logger node)


rebar3 shell --sname loggyRuleta --setcookie secret
loggy:start(loggyRuleta, ['server1@bolddell', 'server2@bolddell','server3@bolddell']).

*NOTA*: Deben estar levantado los nodos de ruleta (no starteados).
    rebar3 shell --sname server1 --setcookie secret
    rebar3 shell --sname server2 --setcookie secret
    rebar3 shell --sname server3 --setcookie secret

2) Instance Servers

rebar3 shell --sname server1 --setcookie secret
ruleta:start(server1, [{server2, 'server2@bolddell'}, {server3, 'server3@bolddell'}], {loggyRuleta, 'loggyRuleta@bolddell'}).

rebar3 shell --sname server2 --setcookie secret
ruleta:start(server2, [{server1, 'server1@bolddell'}, {server3, 'server3@bolddell'}], {loggyRuleta, 'loggyRuleta@bolddell'}).

rebar3 shell --sname server3 --setcookie secret
ruleta:start(server3, [{server1, 'server1@bolddell'}, {server2, 'server2@bolddell'}], {loggyRuleta, 'loggyRuleta@bolddell'}).

3) Instance Load Balancer and sync with servers

rebar3 shell --sname loadBalancer --setcookie secret

loadBalancer:start(loadBalancer, [{server1, 'server1@bolddell'}, {server2, 'server2@bolddell'}, {server3, 'server3@bolddell'}], {loggyLB, 'loggyLB@bolddell'}).

4) Can we do request to ruleta

rebar3 shell --sname clientA --setcookie secret
usuario:start(clientA, {loadBalancer, 'loadBalancer@bolddell'}).

rebar3 shell --sname clientB --setcookie secret
usuario:start(clientB, {loadBalancer, 'loadBalancer@bolddell'}).