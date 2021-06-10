# Loggy

## Build

    $ rebar3 compile

# Introducción

La idea del presente trabajo es la de desarrollar un logger que garantice el orden en que se imprimen por pantalla los mensajes enviados por un conjunto de workers (procesos).

Nos encontramos con una problemática, la cual es que no podemos determinar cuando un mensaje debe ir primero que otro, es decir, cuando uno sucede primero. Para resolver esto, utilizaremos la sincronización a través de relojes lógicos de Lamport.

# Tiempo Lamport

En esencia, el tiempo de lamport es un algoritmo que intenta dar solución a determinar el orden de los eventos de un sistema distribuido. Según el algoritmo, la sincronización de relojes no tiene que ser sobre tiempos absolutos, dado que dos procesos que no interactúan no necesitan que sus relojes estén sincronizados. Sin embargo, sí importa que estén de acuerdo en la hora para coincidir en el orden de los eventos. Por lo tanto, el algoritmo consiste en implementar un reloj lógico. Esto es una estructura de datos que propone Lamport, que es básicamente un simple contador con una función de incremento. Y vamos a decir que un evento ocurre antes que otro cuando ese tiempo lógico en el reloj lógico (contador) sea menor.

---

# Implementación del módulo Time

La primera implementación del Time fue de valores numéricos enteros simples. Y mantuvimos esta implementación por simplicidad en la mayoria de sus funciones.

¿Cómo sabemos que fueron impresos fuera de orden? ¿Cómo identificamos mensajes que estén en orden incorrecto? ¿Qué es siempre verdadero y qué es a veces verdadero?

En primer lugar, identificamos que es incorrecto el orden si vemos que el primer mensaje es un "recieved" antes de un "sending", junto al valor del Time del worker que envía debe ser menor al worker que lo recibe.
Siempre es verdadero que los mensajes de un determinado worker va a estar ordenado para ese worker

¿Cómo sabemos si los mensajes son seguros de imprimir?

Podríamos, siempre que recibimos un mensaje de "recieved" esperar por uno de "sending", y luego ordenar los logs por tiempo. Sin embargo, no podemos garantizar que todos los workers hayan enviado rápidamente los logs y estos se impriman correctamente ordenados. Esto último, se solucionará con lo propuesto más adelante.

# Reporte del módulo time y cola de retención:

En un principio, el módulo time era lo más simple posible, es decir, que se implementó con funciones que retornaban números enteros positivos a los workers.
Luego fuimos agregando las funciones para el logger. Donde ahí manejamos tantos Nodos como su tiempo, en formato de conjunto de dupla.

Si, encontramos entradas fuera de orden. Nos dimos cuenta por el valor del tiempo cuando se imprimían los mensajes del logger en el test.

¿Qué es lo que el log final nos muestra? ¿Los eventos ocurrieron en el mismo orden en que son presentados en el log?

Sin los cambios que implementamos, el log imprimiría simplemente el orden en que le llegan los mensajes al logger, según su cola de mensajes. Con nuestro desarrollo lo que hacemos es esperar para imprimir los mensajes en el orden en que sucedieron los eventos en los demás procesos. Los eventos ocurren con los tiempos de los workers, y por eso el logger debe mantener la cuenta de estos tiempos para luego imprimir los mensajes en el orden correcto. Con respecto a esto último, tuvimos complicaciones que luego describiremos al final en otra sección.

## Longitud de la cola de retención

¿Qué tan larga será la cola de retención?

Con respecto al tamaño de la cola de retención encontramos que cuando menor sea el `jitter` más probabilidades hay de encolar algún mensaje que no sea seguro de imprimir. En cambio, con un `jitter` de medio segundo, lo que encontramos fue una cola que como máximo tenía 15 elementos.

# Dificultades:

Nos encontramos con un bug cuando ejecutamos la prueba que se nos dio. Este bug consiste en que había un log que no se imprimía en orden junto a todos los logs seguros. Por suerte, gracias a la ayuda de los profesores, nos pudimos dar cuenta de que era un log que se imprimía sin ordenarse en la cola pendiente de mensajes. Por lo tanto, lo pudimos solucionar rápidamente.

Otra dificultad que encontramos fue que los mensajes dentro del log estaban ordenados por tiempo, pero no semanticamente, es decir, los mensajes de received podrian estar antes que los de sending. Modificando la funcion de sort que cuando los tiempos logicos son iguales desempate por este mensaje y que si es `sending` vaya primero.

# Vectores de relojes

Si bien con relojes lógicos de Lamport se logra ordenar los mensajes, el orden que propone sobre los mensajes implica que existan mensajes que podrían ser impresos, al no verse afectados entre sí, pero que la implementación simple con estos relojes no permite detectar.

Una mejora sobre esto es la utilizacion de relojes vectoriales, que propone un orden parcial sobre los mensajes. De esta manera, un mensaje va a esperar a ser impreso unicamente cuando el mensaje de un nodo que lo involucra todavia no ha llegado al logger. Esto nos muestra mejor la dependencia que existe entre los distintos nodos, y como se afectan entre si.
