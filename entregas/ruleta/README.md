Ruleta
=====

An OTP library

Build
-----

    $ rebar3 compile


Script
-------

1) Instance Servers

rebar3 shell --sname server1 --setcookie secret
ruleta:start(server1, [{server2, 'server2@bolddell'}, {server3, 'server3@bolddell'}]).

rebar3 shell --sname server2 --setcookie secret
ruleta:start(server2, [{server1, 'server1@bolddell'}, {server3, 'server3@bolddell'}]).

rebar3 shell --sname server3 --setcookie secret
ruleta:start(server3, [{server1, 'server1@bolddell'}, {server2, 'server2@bolddell'}]).

2) Instance Load Balancer and sync with servers

rebar3 shell --sname loadBalancer --setcookie secret

loadBalancer:start(loadBalancer, [{server1, 'server1@bolddell'}, {server2, 'server2@bolddell'}, {server3, 'server3@bolddell'}]).

3) Can we do request to ruleta

rebar3 shell --sname clientA --setcookie secret
usuario:start(clientA, {loadBalancer, 'loadBalancer@bolddell'}).

rebar3 shell --sname clientB --setcookie secret
usuario:start(clientB, {loadBalancer, 'loadBalancer@bolddell'}).