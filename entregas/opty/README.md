# Opty

## Useful commands

- Build

  > $ rebar3 compile

- Shell

  > $ rebar3 shell

- Recompile inside shell

  > 1>r3:do(compile)

## Desarrollo

El trabajo de esta semana consistió en la implementación de un store distribuido y transaccionado. Este store es similar a un array y cada entry es un proceso independiente. También, existe una cantidad fija de los mismos, definida en la creación del store.

A su vez existe un servidor a la espera de clientes que abren nuevas transacciones, donde estas transacciones son controladas por los handlers.
Por último, agregamos un benchmark para poder realizar fácilmente pruebas con sus resultados.

## Dificultades

Una de las dificultades que tuvimos fue simular el caso de que varios clientes quieran acceder al mismo tiempo al mismo recurso y su efecto, pero cuando pudimos simularlo con el benchmark retornaban abort, y al final nos quedó claro.

## Resultados de Pruebas

### Performance

La performance del servidor varía dependiendo de cuántos más clientes hay, más tarda en responder a cada uno. El tamaño del store deducimos que no influye en estos tiempos de respuesta. Un test con 100000 procesos demora aproximadamente entre 280-310 ms aproximadamente en completarse cada uno.

### Limitaciones

¿Cuáles son las limitaciones en el número de transacciones concurrentes y la tasa de éxito?

La tasa de éxito depende altamente de la relación entre la cantidad de transacciones y el tamaño del Store. Para aumentar la tasa de éxito, usamos escrituras en posiciones aleatorias del store, simulando de esta manera un uso más realista.

### Características y Críticas

La implementación no es acorde con una implementación real y eficiente. El validador es un cuello de botella porque es un único proceso validando los commits de todos los usuarios. Mientras más clientes hacen commit, más se demora en responder.

El handler ejecuta en el cliente, y se le pasan el PID del servidor y del validador. Si se quiere distribuir el trabajo deberían crearse varios servidores con sus validadores, que seguramente posean una copia del store para trabajar de forma más eficiente.

Una de las ventajas es que el proceso del handler al vincularse con el cliente muere si el cliente muere. Sin embargo, una de las peores desventajas es que el servidor no tiene control sobre el handler, por lo tanto, es posible abrir un número descontrolado de handlers, o bien que un mismo cliente abra la cantidad de handlers que este desee, sin ninguna restricción.
