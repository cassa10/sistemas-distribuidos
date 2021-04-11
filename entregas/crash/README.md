Solución Crash
=====

Tuvimos problemas con el ejercicio de los nodos (Programación Distribuida), hasta que nos dimos cuenta que habia que registrar el proceso con el nombre del primer parametro de la funcion register.


# Ping-pong

Es necesario empezar por el Nodo-A.

### Nodo-A
1. Ir al directorio entregas/crash.
2. Ejecutar en ese directorio: 
    
    ```rebar3 shell --sname <*node-a-name*> --setcookie <*cookie*>```

    Ejemplo:  
    ```rebar3 shell --sname pong --setcookie secret```
3. Ejecutar el comando:
    
``` erlang
    pingpong:start_pong().
```

4. Ir a la seccion Nodo-B

### Nodo-B

1. Ir al directorio entregas/crash.
2. Ejecutar en ese directorio: 
    
    ```rebar3 shell --sname <*node-b-name*> --setcookie <*cookie*>```

    Ejemplo:  
    ```rebar3 shell --sname ping --setcookie secret```
3. Ejecutar el comando:
    
``` erlang
    pingpong:start_ping(N, NodeA).
```

Donde: 
* *NodeA* es un string que identifica el nodo A. (Se puede obtener, utilizando la funcion ```node()``` dentro del nodo) 

* *N* es un numero, el cual va a ser la cantidad de pings que se van a mandar al NodoA.

Ejemplo:

``` erlang
    pingpong:start_ping(7, 'pong@my_computer').
```

## Problemas

* Siempre hay que inicializar primero el nodo A junto la funcion start_pong(), para que el proceso del Nodo-B no quede obsoleto. Habria que buscar una forma de sincronizacion, cuando sucede este caso.




