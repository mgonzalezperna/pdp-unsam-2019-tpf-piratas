## Juego de piratas en Haskell

Basado en un punto de la consigna que referia a contar la historia de un pirata, nació la idea de hacer un piping de funciones para relatar una verdadera historia, genuina e irrepetible en cada iteración.
En éste sencillo juego encarnamos un pirata que decide emprender una aventura en busqueda de convertise en una leyenda de los 7 mares.

### Pasos previos.

* Instalar plataforma Haskell. (https://www.haskell.org/downloads/) 
* Instalar el paquete *random*. Para esto, desde un terminal escribir `cabal v1-install random`
* Clonar repositorio.

### Como jugar.

Dentro del directorio base del repositorio, cambiar al branch *Juego*

`git checkout juego`

Y luego correr:

`ghci Juego.hs`

El CLI de Haskell mostrará que el juego fue compilado con éxito.
Dentro de la consola de Haskell, tipear:

`comenzar_historia`

El juego comenzará. Disfrutalo!

Para salir del juego, podés presionar *Ctrl+C* en cualquier instante.
Para salir del CLI de Haskell, podés tipear *:q* en la consola.

