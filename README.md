# Trabajo Grupal Haskell
>Acá subiremos una descripción lo que ocurre en nuestro codigo. Será nuestra forma de tambien 'documentar' el codigo.



### Ejercicio 6

> Parametros :
>   - **vuelos** (AgengiaDeViajes).
>   - **ciudadOrigen** -$>$ de donde partimos.
>   - **ciudadDestino** -$>$ a donde queremos ir.

> Tomando lista de **"vuelos"** conformada por **("origen", "destino", "tiempo")$_{n}$** / $n \in $ {$1.. x$}, $x$ = longitud de **vuelos**

> Los viajes pueden ser **"directos"** o con una **"escala"**

> Un viaje con **escala** va a tener el la **ciudadOrigen** en un $vuelo_n$ y un la **ciudadDestino** en el $vuelo_m$ y con una ciudad en común, sea esta:
>   - Destino de $vuelo_n$ y origen de $vuelo_m$ 
>   - Destino de $vuelo_m$ y origen de $vuelo_n$
>
> / $n \not= m$ 

#### pasos a seguir:
1. filtramos aquellos con origen y/o destino iguales a **ciudadOrigen** y **ciudadDestino**  usando la funcion **destinoUOrigenEnComun** (para ahorrar trabajo de computo en los siguientes puntos)
``` hs
destinoUOrigenEnComun :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
destinoUOrigenEnComun [] _ _ = [] --caso base
destinoUOrigenEnComun ((origen, destino, tiempo):vuelos) ciudadOrigen ciudadDestino = 
    {-
        proceso recursivo de validación ciudadOrigen/ciudadDestino en origen/destino de vuelo respectivamente.
    -}{-
        De una lista de [(origen,destino,tiempo)_1...(origen,destino,tiempo)_n] nos quedamos solo con aquellos que cumplan lo descrito.
    -}

```
2. mediante otra funcion **tiempoDeVueloDirectoOEscala** tomamos la lista con 3 casos posibles (incluye 2 base)
``` hs
tiempoDeVueloDirectoOEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
tiempoDeVueloDirectoOEscala [] _ _ = [] -- caso base
tiempoDeVueloDirectoOEscala [(origen, destino, tiempo)] ciudadOrigen ciudadDestino =
    {-
        valdrá acorde al caso:
            1. [tiempo] <=> origen == ciudadOirgen && destino == ciudadDestino
            2. [] (else)
    -}

    {- se llamará "vuelo1" al (origen1,destino1,tiempo1) y "vuelo2" al (origen2,destino2,tiempo2) -}
tiempoDeVueloDirectoOEscala ((origen1, destino1, tiempo1):(origen2, destino2, tiempo2):vuelos) ciudadOrigen ciudadDestino =
    {-
        3 casos posibles
            1. vueloDirecto (vuelo1 da directo con ciudadOrigen y ciudadDestino)
                devolviendo = tiempo1 : (llamado recursivo con (vuelo2:vuelos) ciudadOrigen ciudadDestino)
            
            2. juntasDanConDestino (el conjunto de vuelo1 y vuelo2 cumple que son de origen diferente, destino diferente, pero matchean uno de los dos con el origen y el otro con el destino. a su vez, tienen el termino restante (destino de uno y origen del otro) iguales.
            
            caso ejemplo : 
            [("BS AS", "Santa Fe", tiempo1), ("Santa Fe", "Neuquen", tiempo2)] (con CO = "BS AS" y CD = "Neuquen"). 
            
            las mismas tiene origen/destino diferentes, uno matchea con la ciudad de origen ("BS AS") y el otro con el destino ("Neuquen"), a su vez tienen una ciudad en comun ("Santa Fe", destino de 1 y origen de 2).)

            Esta devuelve = (tiempo1 + tiempo2) : (llamado recursivo vuelos ciudadOrigen ciudadDestino)

            3. otherwise. Se toman ambos Viajes y se corrobora recursivamente si matchean con los siguientes en vuelos

            Esta devuelve = (llamado recursivo1 (vuelo1:vuelos) ciudadOrigen ciudadDestino) ++ (llamado recursivo2 (vuelo2:vuelos) ciudadOrigen ciudadDestino)
    -}
```
con esta funcion se logra recorrer y validad si es que cada vuelo existente puede tener otro vuelo con el que, haciendo escala, llegue de origen a destino, o de otro modo, que entre los vuelos existente pueda uno llegar a destino de forma directa del origen.

3. funcion de valor minimo
``` hs
minimaDuracion :: [Duracion] -> Duracion
minimaDuracion [tiempo] = tiempo -- caso base. ultimo tiempo restante en tiempos, siendo el menor de todos.
minimaDuracion  (tiempo1:tiempo2:tiempos) 
    {-
        2 casos:
            1. el tiempo1 <= tiempo2
                devuelve la recursion de minimaDuracion (tiempo1:tiempos)
            2. otherwise
                devuelve la recursion de minimaDuracion (tiempo2:tiempos)
    -}
```
como sabemos, esta funcion recibe de la anterior una lista con los tiempos de cada vuelo (escala o directo) y devuelve la minima duración de vuelo entre los dos puntos definidos como ciudadOrigen y ciudadDestino