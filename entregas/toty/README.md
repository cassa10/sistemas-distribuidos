Toty
=====

An OTP library

Build
-----

    $ rebar3 compile

Shell
------    
    $ rebar3 shell

Recompile inside shell
----------
    >1>r3:do(compile).

Test Toty
----------

    > toty:run(Jitter, Sleep).

Obs: where Jitter and Sleep are Integers


## Introducción

En este trabajo desarrollamos un multicast entre cuatro workers que desean consensuar un color (RGB) para mostrar por UI. Para consensuar un color, los workers van a proponer algún número, el cual va a servir para el cálculo de determinación del color a mostrar. Este cálculo es simplemente: ```f({ R, G, B }, N) -> { G, B, (R+N) rem 256 }```.

La implementación se basa en que cada worker va a tener su propio proceso “multicast” que manejará la lógica de enviar el mensaje deseado y consensuar con los demás workers.

El color inicial de los workers es el negro `{R, G, B} = {0, 0, 0}`, y lo que debemos confirmar es que los workers reciban los mensaes en su orden correspondiente, donde todos los workers pasen por los mismos colores y estos no difieran. 

## Implementación

Para la primera implementación, lo que hicimos fue pasar un tiempo de `Sleep` al worker para que este espere antes de mandar un mensaje y uno de `Jitter` al proceso de multicast para que una vez que reciba el mensaje del worker, espere para enviarlo a las demás partes que conoce.
El proceso multicast recibe a su master en un primer mensaje, y luego sus nodos pares en otro, y desde ahí pasa al estado `server` en el que espera tanto mensajes de su worker. Como mensajes de sus pares, que entrega al worker apenas recibe.

Luego, agregamos y cambiamos algunas cosas de esta primera implementación, como por ejemplo, en el valor de secuencia solo mantenemos un número, ya que no hace falta que utilicemos el id. 

Otra de las cosas, son los valores de la `Queue` que tiene un campo más, que es un atom que varía entre `proposal`, este se asigna cuando se encola por primera vez, y el otro valor es `agreed`, que se asigna una vez que se obtiene el consenso, esto nos permite filtrar los que ya podemos enviar a nuestro worker, aunque siempre son ordenados por el valor consensuado.

Notamos que es posible que un mensaje de un worker empiece a acordarse antes que otro, pero este último puede ser entregado antes, si consigue el acuerdo primero. Por eso es posible que varias ventanas se empiecen a pintar del color que uno de los procesos multicasts propuso, y otro comience a pintarse en otro de esos colores.

## Conclusión

Aprendimos otra implementación de multicast de mensajes, en el cual los procesos multicast se ponen de acuerdo para asignar un orden total a los mensajes, y de esta manera, todos lo respeten. Esto es necesario, ya que la función de los workers no es equivalente con diferentes tipos de orden, es decir, no es conmutativa e importa dicho orden.

De esta manera, los procesos multicast quedan a la espera de los mensajes de su worker, para luego enviar un request a sus pares, los cuales esperan las propuestas de orden de cada uno. Una vez que se reciben todas las propuestas es donde se llega a un consenso o acuerdo. 

Notamos que esta implementación funciona en la mayoría de casos, y pudimos darnos cuenta algunos otros en los que no. La falla por lo general que encontramos fue cuando hacíamos menor los tiempos de Sleep y Jitter (variables que manejan las demoras en el envío y entrega de los mensajes).

En conclusión, fue una actividad en donde no tuvimos el mismo soporte por parte del enunciado con respecto a anteriores trabajos. Es decir, que la gran mayoría del trabajo la tuvimos que pensar y deducir. Sin embargo, pudimos basar parte de la arquitectura en el trabajo anterior `“Muty”`. Creemos que llegamos a una posible solución, pero podría darse el caso que algunas de las fallas que encontramos se deban a errores en nuestro código.
