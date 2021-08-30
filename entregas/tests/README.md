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

- ```testServer:start(N, [{X, 'serverX@my_computer'}, {Y, 'serverY@my_computer'}])```

Una vez creado los nodos con los server instanciados con sus respectivos "Id" de proceso, habria que instanciar el Load Balancer para que les de la orden a cada servidor si es `master` o `slave`, para empezar a brindar sus servicios.


2) Instanciar Load Balancer en una terminal diferente

- ```rebar3 shell --sname loadBalancer --setcookie secret```

- ```testLoadBalancer:start(Id, [{N, 'serverN@my_computer'}, {X, 'serverX@my_computer'}, {Y, 'serverY@my_computer'}])```

Una vez instanciado el load balancer, ahora uno de los servidores empezara a tomar el rol de master (en nuestro codigo seria el primer elemento de la lista de nodos) y los demas de slaves, los cuales replicaran al master en cada evento o accion que tome este primero.

3) Instanciar los clientes necesarios para probar

```rebar3 shell --sname client --setcookie secret```

```testClient:apostar({Id, 'loadBalancer@my_computer'})```

