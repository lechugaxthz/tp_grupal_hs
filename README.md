# Trabajo Grupal Haskell
>Acá subiremos una descripción lo que ocurre en nuestro codigo. Será nuestra forma de tambien 'documentar' el codigo.



## Ejercicio 6

### Parametros :
>   - **vuelos** (_AgengiaDeViajes_).
>   - **ciudadOrigen** -$>$ de donde partimos.
>   - **ciudadDestino** -$>$ a donde queremos ir.

> Tomando lista de **"vuelos"** conformada por **("origen", "destino", "tiempo")$_{n}$** / $n \in $ {$1.. x$}, $x$ = longitud de **vuelos**

> Los viajes pueden ser **"directos"** o con una **"escala"**

> Un viaje con **escala** va a tener el la **ciudadOrigen** en un $vuelo_n$ y un la **ciudadDestino** en el $vuelo_m$ y con una ciudad en común, sea esta:
>   - Destino de $vuelo_n$ y origen de $vuelo_m$ 
>   - Destino de $vuelo_m$ y origen de $vuelo_n$
>
> / $n \not= m$ 

### pasos a seguir:
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

---
## Ejercicio 7

### Descripción :
> Funcion : _puedoVolverOrigen_
> Cumple : Devuelve un Booleano acorde a si es posible salir con un vuelo desde el _Origen_ hasta otro destino _X_ y desde ese destino volver al _Origen_ de con otro vuelo o vuelos desde _X_ a _Y_ y siguientes hasta _Origen_ (es decir, con una o mas _Escalas_)

### Funciones pricipales y secundarias :

#### puedoVolverAOrigen
##### Parametros y valor:
>   - Parametros
>       - **vuelos** (_AgenciaDeViajes_)
>       - **origen** -$>$ ciudad de la que queremos salir y volver en otro u otros vuelos
>
>   - Valor
>       - La función vale un Booleano acorde a si cumple o no con lo indicado.

##### Acciones :
> Llama a _validaEscalas_ y otras funciones (_conMismoOrigen_, _diferenteACiudad_, _conMismoDestino_) como parte de su parametro para obtener un tipo de dato _[[AgenciaDeViajes]]_ (se describen las mismas funciones a continuación)

##### Función :
``` hs
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigen vuelos origen = validaEscalas [conMismoOrigen vuelos origen, diferenteACiudad vuelos origen, conMismoDestino vuelos origen]
```
---
#### conMismoOrigen, conMismoDestino, diferenteACiudad
##### Parametros y valor:
>   - Parametros
>       - **((origen, destino, t) : viajes)** /
>           - **origen** -$>$  _Ciudad_
>           - **destino** -$>$  _Ciudad_
>           - **t** -$>$  _Duracion_
>           - **viajes** -$>$ _AgenciaDeViajes_
>       - **ciudad** -$>$ _Ciudad_ 
>
>   - Valor
>       - **conMismoOrigen** -$>$  &#8704; x $\in$ [AgenciaDeViajes] / x.origen == ciudad
>       - **conMismoDestino** -$>$  &#8704; x $\in$ [AgenciaDeViajes] / x.destino == ciudad
>       - **diferenteACiudad** -$>$  &#8704; x $\in$ [AgenciaDeViajes] / x.origen /= ciudad $\And\And$ x.destino /= ciudad

##### Actiones :
> Estas funciones son independientes de otras pero son llamadas en multiples ocaciones:
>   - Inicio de la ejecución.
>       - Como vimos en la función anterior [puedoVolverAOrigen], llamamos a estas 3 funciones con el fin de tener, de modo mas organizado, la información de los vuelos para su posterior tratado, dejandonos con una lista del siguiente tipo para entregar a la función **validaMasEscalas** : [[AgenciaDeViajes]], tiendo como resultado -$>$ [[conMismoOrigen], [diferenteACiudad], [conMismoDestino]]
>   - Posteriormente
>       - Solo 2 funciones (**conMismoOrigen** y **conMismoDestino**) son llamadas posteriormente con el fin de hacer el/los llamados recursivos necesarios dentro de la función **validaMasEscalas** junto con otra función (**diferenteA2Ciudades**). Estos serán descritos en codigo para menor confución en [anotar mas tarde]

##### Funciones :

``` hs
conMismoOrigen :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
conMismoOrigen [] _ = [] -- caso base
conMismoOrigen ((origen, destino, t) : vuelos) ciudad
    {-
        devuelve aquellos viajes (como "(origen, destino, t)") que matcheen "origen" con "ciudad" con la recursión conMismoOrigen vuelos ciudad
    -}
```
``` hs
conMismoDestino :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
conMismoDestino [] _ = [] -- caso base
conMismoDestino ((origen, destino, t) : vuelos) ciudad
    {-
        devuelve aquellos viajes (como "(origen, destino, t)") que matcheen "destino" con "ciudad" con la recursión conMismoDestino vuelos ciudad
    -}
```
``` hs
diferenteACiudad :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
diferenteACiudad [] _ = [] -- caso base
diferenteACiudad ((origen, destino, t) : vuelos) ciudad
    {-
        devuelve aquellos viajes (como "(origen, destino, t)") que no matcheen "origen" ni "destino" con "ciudad" con la recursión diferenteACiudad vuelos ciudad
    -}
```
---
#### diferenteA2Ciudades
##### Parametros y valor:
> Parametros
>   - **((origen, destino, t) : viajes)** /
>       - **origen** -$>$  _Ciudad_
>       - **destino** -$>$  _Ciudad_
>       - **t** -$>$  _Duracion_
>       - **viajes** -$>$ _AgenciaDeViajes_
>   - **ciudad1** -$>$ _Ciudad_ 
>   - **ciudad2** -$>$ _Ciudad_ 
>
> Valor
>   - La función devuelve una lista de aquellos viajes que no presenten en presenten en su **origen** a la **ciudad2** y en su **destino** a la **ciudad1**
##### Actiones :
> Esta función por si es independiente de otras, pero al igual que las [anteriores nombradas](#conmismoorigen-conmismodestino-diferenteaciudad) y es llamada repetidas veces para la recursión de **validaMasEscalas** (explicada mas adelánte)

##### Funciones :
``` hs
diferenteA2Ciudades :: AgenciaDeViajes -> Ciudad -> Ciudad -> AgenciaDeViajes
diferenteA2Ciudades [] _  _ = [] -- caso base
diferenteA2Ciudades ((origen, destino, t) : vuelos) ciudad1 ciudad2
    {-
        devuelve viaje (como "(origen, destino, t)") cuando origen /= ciudad2 && destino /= ciudad1 con la recursión diferenteA2Ciudades vuelos ciudad1 ciudad2
    -}
```
---
#### validaMasEscalas
##### Parametros y valor:
> Parametros
>   - (((origenA , destinoA, tA) : origenIgual) : ((origen, destino, t): diferenteTodo) : ((origenB, destinoB, tB) : destinoIgual) : conjunto) 
>       - **origen$_n$** -$>$  &#8704; n $\in$ {&#8709;, A, B} / origen$_n$ == viaje.origen && viaje $\in$ origenIgual
>       - **destino$_n$** -$>$  &#8704; n $\in$ {&#8709;, A, B} / destino$_n$ == viaje.destino && viaje $\in$ origenIgual
>       - **t$_n$** -$>$  &#8704; n $\in$ {&#8709;, A, B} / t$_n$ == viaje.tiempo && viaje $\in$ origenIgual
>       - **conjunto** -$>$ [[AgenciaDeViajes]]
> Valor
>   - Devuelve un valor Booleano, acorde a si la busqueda de un viaje que de desde un origen definido (por el o los origenes en **origenIgual**) llegan directamente o con escala a un destino igual al origen definido (en este caso, el destino en **destinoIgual**)
>   - 
##### Actiones :
##### Funciones :

