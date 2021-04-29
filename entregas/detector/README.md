# Detector

## Useful commands

- Build

  > $ rebar3 compile

- Shell

  > $ rebar3 shell

- Recompile inside shell

  > 1>r3:do(compile)

## Desarrollo

Comenzamos implementando el producer/consumer tal cual lo indicaba el ejercicio. No
tuvimos complicaciones al respecto. Probamos como se detecta el crash o una falla a partir de los monitores, tambien probamos la desincronizacion de un nodo producer y otro consumer, cuando un consumer se desconecta y se vuelve a contectar, o tarda mas en responderle al producer.

## Dificultades

La dificultad que fue entender el significado del `M > N` que se encuentra en el `validateNumbers(N, M)`, y luego intentar simular esa desincronizacion, ya que nosotros matabamos el consumer y con el producer que se nos dio se quedaba esperando al consumer vinculado y este no se modificar. Por esta razon, se agrego a nuestro codigo un nuevo mensaje `consumerDie` que manda el consumer al producer, el cual significa que el consumer murio y habilita la espera de un nuevo consumer mientra sigue produciendo en la funcion `waitConsumer`.

Hay un pequeño problema en la transicion, en el caso de cuando llega un nuevo consumer y el tiempo de la produccion del producer. Este problema es que se vuelve a resetear perdiendo la marca anterior que ya habia pasado.

Como posible solucion se nos ocurrio hacer un calculo de tiempo para evitar el reseteo entero del delay y no se produzca una perdida.

## Preguntas

### 1. ¿Qué mensaje se da como razón cuando el nodo es terminado?¿Por que?

El mensaje que se da es "noconnection", porque el monitor detecta que el proceso
ya no está corriendo.

### 2. ¿Qué sucede si matamos el nodo Erlang en el producer?

Si matamos el nodo de erlang: `{producer,'maquina1@192.168.0.30'} died; noconnection`.

Si hacemos un crash sobre el producer: `{producer,'maquina1@192.168.0.30'} died; {badarith,[{producer, ...`.

Si el producer todavía no estaba levantado: `{producer,'maquina1@192.168.0.30'} died; noproc ...`.

### 3. Ahora probemos desconectar el cable de red de la máquina corriendo el producer y volvamos a enchufarlo despues de unos segundos. ¿Qué pasa?

Sigue normalmente.

### 4. Desconectemos el cable por períodos mas largos. ¿Qué pasa ahora?

Lo que paso fue que ibamos por el `ping 23`, desconectamos un rato largo (más de 1 min), el monitor no avisaba nada hasta el momento. Volvimos a enchufar y aparecio `{producer,'maquina1@192.168.0.30'} died; noconnection`. Sin embargo, empezo a resolver ping de nuevo, sòlo que estaba desfasado. Esto es así porque el consumer imprimía lo siguiente: `el numero fue mas alto M:45 N:25`. Y siguió así, siempre desfasado por `20`.

### 5. ¿Qué significa haber recibido un mensaje 'DOWN'? ¿Cuándo debemos confiar en el?

Significa que el monitor detectó que el proceso monitoreado está caído por alguna razón, lo que no implica que se pueda conectar nuevamente. Creemos que el mensaje del monitor es sólo informativo, y en algún momento se debe tomar la decisión de dar por muerta la conexión con el otro nodo, como por ejemplo utilizar un timeout.

### 6. ¿Se recibieron mensajes fuera de orden, aun sin haber recibido un mensaje 'DOWN'?

Se recibió un 'DOWN' en un momento, y despues se empezo a recibir mensajes desordenados.

### 7. ¿Qué dice el manual acerca de las garantías de envíos de mensajes?

Se puede asumir que los mensajes se encolan en el orden en que llegaron a su destino, pero eso no significa que mensajes que se hayan enviado de una máquina a otra lleguen efectivamente a destino. En otras palabras, es posible que se pierdan mensajes en la comunicación.
