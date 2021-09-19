tests
=====

An OTP library

Build
-----

    $ rebar3 compile


Test Load Balancer
-------------------

Componentes:

- testServer
- testLoadBalancer
- testClient

1) Instanciar Servers en terminales diferentes

Paso para instanciar server:
    
- ```rebar3 shell --sname serverN --setcookie secret```

- ```testServer:start(serverN, [{X, 'serverX@my_computer'}, {Y, 'serverY@my_computer'}]).```

Ejemplo:

- ```rebar3 shell --sname server1 --setcookie secret```
- ```testServer:start(server1, [{server2, 'server2@bolddell'}, {server3, 'server3@bolddell'}]).```

- ```rebar3 shell --sname server2 --setcookie secret```
- ```testServer:start(server2, [{server1, 'server1@bolddell'}, {server3, 'server3@bolddell'}]).```

- ```rebar3 shell --sname server3 --setcookie secret```
- ```testServer:start(server3, [{server1, 'server1@bolddell'}, {server2, 'server2@bolddell'}]).```

Una vez creado los nodos con los server instanciados con sus respectivos "Id" de proceso, habria que instanciar el Load Balancer para que les de la orden a cada servidor si es `master` o `slave`, para empezar a brindar sus servicios.


2) Instanciar Load Balancer en una terminal diferente

- ```rebar3 shell --sname loadBalancer --setcookie secret```

- ```testLoadBalancer:start(Id, [{N, 'serverN@my_computer'}, {X, 'serverX@my_computer'}, {Y, 'serverY@my_computer'}])```

Ejemplo:

- ```rebar3 shell --sname loadBalancer --setcookie secret```

- ```testLoadBalancer:start(loadBalancer, [{server1, 'server1@bolddell'}, {server2, 'server2@bolddell'}, {server3, 'server3@bolddell'}]).```


Una vez instanciado el load balancer, ahora uno de los servidores empezara a tomar el rol de master (en nuestro codigo seria el primer elemento de la lista de nodos) y los demas de slaves, los cuales replicaran al master en cada evento o accion que tome este primero.

3) Instanciar los clientes necesarios para probar

- ```rebar3 shell --sname client --setcookie secret```

- ```testClient:apostar(id, {Id, 'loadBalancer@my_computer'}, N).```

Ejemplo:

- ```rebar3 shell --sname client --setcookie secret```
- ```testClient:apostar(client, {loadBalancer, 'loadBalancer@bolddell'}, 110).```



Script:

#Servers

rebar3 shell --sname server1 --setcookie secret
testServer:start(server1, [{server2, 'server2@bolddell'}, {server3, 'server3@bolddell'}]).

rebar3 shell --sname server2 --setcookie secret
testServer:start(server2, [{server1, 'server1@bolddell'}, {server3, 'server3@bolddell'}]).

rebar3 shell --sname server3 --setcookie secret
testServer:start(server3, [{server1, 'server1@bolddell'}, {server2, 'server2@bolddell'}]).

#Load Balancer

rebar3 shell --sname loadBalancer --setcookie secret

testLoadBalancer:start(loadBalancer, [{server1, 'server1@bolddell'}, {server2, 'server2@bolddell'}, {server3, 'server3@bolddell'}]).

#Client

rebar3 shell --sname clientA --setcookie secret
testClient:apostar(clientA,{loadBalancer, 'loadBalancer@bolddell'},110).

rebar3 shell --sname clientB --setcookie secret
testClient:apostar(clientB,{loadBalancer, 'loadBalancer@bolddell'},500).

rebar3 shell --sname clientC --setcookie secret
testClient:apostar(clientC,{loadBalancer, 'loadBalancer@bolddell'},3000).