## Juego de piratas en Haskell

Basado en un punto de la consigna que referia a contar la historia de un pirata, nació la idea de hacer un piping de funciones para relatar una verdadera historia, genuina e irrepetible en cada iteración.
En éste sencillo juego encarnamos un pirata que decide emprender una aventura en busqueda de convertise en una leyenda de los 7 mares.

### Pasos previos.

* Instalar Stack (https://docs.haskellstack.org/en/stable/install_and_upgrade/). En Sistemas Operativos Unix type se puede hacer usando:
```wget -qO- https://get.haskellstack.org/ | sh)```
* Clonar repositorio.

### Como jugar.

Dentro del directorio base del repositorio, cambiar al branch *Juego*

`git checkout juego`

Luego ejecutar 

```stack build```

Finalmente correr el juego usando ```stack exec juego-piratas-exe```

El juego comenzará. Disfrutalo!

Para salir del juego, podés presionar *Ctrl+C* en cualquier instante.
