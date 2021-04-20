Rudy
=====


Useful commands
-----
- Build
    
    >$ rebar3 compile

- Shell
    
    >$ rebar3 shell

- Recompile inside shell

    >1>r3:do(compile)

## Desarrollo

Comenzamos implementando el servidor tal cual lo indicaba el ejercicio. No
tuvimos complicaciones al respecto. Probamos hacer peticiones desde una consola
en otra pc y desde el navegador.

## Preguntas
Las preguntas que respondimos son las siguientes:

### a) ¿cuántos requests por segundo podemos servir?

#### Sin Delay

116789μs(0,12s aprox).

#### Con Delay

4259658μs(4.25s aprox).

### b) ¿Nuestro delay artificial es importante o desaparece dentro del overhead de parsing? 

Parece importar dado que los tiempos de respuesta empeoran enórmemente en presencia del delay.

### c) ¿Qué ocurre si ejecutamos los benchmarks en varias máquinas al mismo tiempo? 

En el caso sin delay, los tiempos de respuesta se mantienen. En el caso de tener el delay,
los tiempos de respuesta empeoran aún más. 

### Benchmarks

TODO

## Discusión

### a) ¿Deberíamos crear un nuevo proceso por cada request de entrada? 

Si, siempre y cuando la cantidad de request de entrada, sea acorde, a la cantidad de concurrencia que soporta la cpu.

### b) ¿Toma tiempo crear un nuevo proceso? 

No.

### c) ¿Qué ocurriría si tenemos miles de requests por minuto?

Dependiendo de la cantidad de procesos que tengamos, puede tardar mas o menos.
