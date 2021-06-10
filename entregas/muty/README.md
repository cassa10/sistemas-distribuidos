muty
=====

An OTP library

Build
-----

    $ rebar3 compile

# Introducción

El presente trabajo consistió en realizar diferentes algoritmos de lock de exclusión mutua distribuido. Hay workers que compiten por obtener la sección crítica, en este ejercicio una `gui`, y cada uno posee un lock. Estos locks se envían mensajes para acordar quién de ellos va a dejar ingresar a su worker a la sección crítica. Para que no caiga en un deadlock, los workers poseen un timer para comunicarle a su lock que libere su puesto por la sección crítica, y así evitar un posible deadlock.

Los Locks poseen 3 estados: open, waiting y held. Cuando un lock esta en open, el lock está esperando que su worker le mande un mensaje de take para cambiar al estado waiting, y así consultarle a todos sus locks que emparejó (conoce) si puede dejar a su worker ingresar a la sección crítica. Una vez conseguido todas las confirmaciones de sus locks, el lock pasa al estado held, donde también puede recibir requests de otros locks y los agrega a la lista de locks que están esperando su confirmación.

El lock en los estados held y waiting se puede cansar de esperar, o bien termina su trabajo. Sin embargo, en cualquiera de estos dos casos le avisa a su lock un mensaje de release que ya no le interesa ingresar a la sección crítica. En este último caso, el lock le envía un mensaje a todos sus locks que están en espera. Después de esto entra al estado open.

Vamos a tener 3 implementaciones de locks diferentes.

# Lock 1

Esta implementación de lock es básica y simple, solo evita que no se acceda a la sección crítica concurrentemente. Además, fue provisto por el enunciado, y no tuvimos problemas en su implementación. 

Los mensajes de request en el estado held son recibidos como describimos en la sección anterior. Sin embargo, se nos preguntó si sería posible utilizar la misma cola de mensajes de Erlang hasta liberarse el lock, y luego simplemente ir desencolandolas. La respuesta a esto es si, aunque creemos que es interesante no encolar tantos mensajes de request, para no saturar la cola de estos.

Por otro lado, se consultó también si en el estado held tiene sentido estar esperando mensajes de ok. La respuesta a esto sería que no, ya que recibimos todos los ok que queríamos al entrar en ese estado particular.

# Lock 2

En esta otra implementación, se utiliza el Id del lock recibido por parámetro en su construcción. Este Id en la primera implementación no tiene sentido, pero se lo damos igual para respetar una misma interfaz.

Este Id se utiliza primordialmente en el estado de waiting, ya que si recibimos un request, tenemos que comparar si el Id recibido del request tiene más prioridad que nuestro Id. En caso de que sea prioritario, le damos el ok al proceso que pide el request y le hacemos una nueva request a este mismo proceso, por si se llega a dar el caso de que ambos están respondiendo al mismo tiempo, y así evitar una posible entrada múltiple a la sección crítica. Pero, en caso contrario, lo hacemos esperar debido a que nosotros somos más prioritarios. Si los Id son iguales decidimos hacerlo esperar también, dado que sino dos procesos podrían entrar a la sección crítica concurrentemente.

Se verificó que este algoritmo funcione, pero tiene una desventaja muy grande, el cual es que un lock con prioridad alta tendría mayor porcentaje de entrada en la sección crítica, y los de menos prioridad menor probabilidad de conseguirlo. Esto podría darse en un starvation (nunca ejecutar en la sección crítica) por parte de los looks de baja prioridad.

¿Podemos garantizar que tenemos un solo proceso en la sección crítica en todo momento?

Suponemos que sí, ya que al competir por el recurso el algoritmo utilizado es similar al anterior, y si hay algún proceso en la sección crítica, este no va a mandar ok a nadie.

No encontramos grandes cambios de eficiencia en comparación a la implementación del Lock-1. Sin embargo, suponemos que con un gran número de locks, la competencia por la sección crítica debería terminar mucho antes, ya que si los Id son diferentes, uno será más prioritario que el resto y entrara más rápido a la sección crítica.


# Lock 3



# Problemas


# Conclusión

