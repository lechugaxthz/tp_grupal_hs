module Solucion where

type Ciudad = String

type Duracion = Float

type Vuelo = (Ciudad, Ciudad, Duracion)

type AgenciaDeViajes = [Vuelo]

-- EJERCICIO 1
vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos _ = True -- Borrar y escribir el código correcto

-- EJERCICIO 2
ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas [] _ = [] -- Caso base 
ciudadesConectadas [(c1,d1,t1)] ciudad  -- Caso donde en AgenciaDeViajes solo hay 1 elemento
    | ciudad == c1 = [d1]  -- Si la ciudad es la de origen me da el destino
    | ciudad == d1 = [c1]  -- Si la ciudad es la de destino me da la de origen
    | otherwise = [] -- Si la ciudad no está conectada no devuelve ninguna 
ciudadesConectadas ((c1,d1,t1): vuelos) ciudad -- Caso donde AgenciaDeViajes tiene más de 1 elemento
    | ciudad == c1 = sacarCiudadesRepetidas([d1] ++ ciudadesConectadas vuelos ciudad)
    | ciudad == d1 = sacarCiudadesRepetidas([c1] ++ ciudadesConectadas vuelos ciudad)
    | otherwise = ciudadesConectadas vuelos ciudad 

sacarCiudadesRepetidas :: [Ciudad] -> [Ciudad]
sacarCiudadesRepetidas [] = []
sacarCiudadesRepetidas [x] = [x]
sacarCiudadesRepetidas (x:xs)
    | not (pertenece1 x xs) = [x] ++ sacarCiudadesRepetidas xs
    | otherwise = x : (sacarCiudadesRepetidas (sacarCiudadEspecifica x xs)) 

pertenece1 :: Ciudad -> [Ciudad] -> Bool -- Función que se fija si la ciudad pertenece a la lista de ciudades
pertenece1 _ [] = False
pertenece1 x (y:ys) = x == y || pertenece1 x ys

sacarCiudadEspecifica :: Ciudad -> [Ciudad] -> [Ciudad]
sacarCiudadEspecifica _ [] = []
sacarCiudadEspecifica x (y:ys)
    | x == y = sacarCiudadEspecifica x ys
    | otherwise = y : (sacarCiudadEspecifica x ys)

-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota ((origen, destino, tiempo) : vuelos) = (origen, destino, tiempoActualizado) : modernizarFlota vuelos
  where
    tiempoActualizado = tiempo * 0.9

-- EJERCICIO 4
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada _ = "Rosario" -- Borrar y escribir el código correcto

-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ = False
sePuedeLlegar ((c1,d1,t1):xs) origen destino =  conUnViaje ((c1,d1,t1):xs) origen destino || conEscala (conMismoOrigen((c1,d1,t1):xs) origen) (conMismoDestino((c1,d1,t1):xs) destino) 

conUnViaje :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
conUnViaje [] _ _ = False
conUnViaje ((c1,d1,t1):xs) origen destino 
    | origen == c1 && destino == d1 = True
    | otherwise = conUnViaje xs origen destino 

conEscala :: AgenciaDeViajes -> AgenciaDeViajes -> Bool
conEscala [] _ = False
conEscala _ [] = False
conEscala ((c1,d1,t1):xs) ((c2,d2,t2):ys) 
    | d1 == c2 = True
    | otherwise = conEscala ((c1,d1,t1):xs) ys || conEscala xs ((c2,d2,t2):ys)

-- EJERCICIO 6

{-
  explicación:
    - viajesFiltrados. tal como dice la función, solo toma aquellos viajes de "interes"
    - listaDeTiemposDeVuelo. aquellos vuelos que coincídan sea de forma directa u escala, son almacenados sus tiempos en una lista
-}
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido viajes ciudadOrigen ciudadDestino = minimaDuracion listaDeTiemposDeVuelo
  where
    viajesFiltrados = destinoUOrigenEnComun viajes ciudadOrigen ciudadDestino
    listaDeTiemposDeVuelo = tiempoDeVueloDirectoOEscala viajesFiltrados ciudadOrigen ciudadDestino

{-
  Filtra por los vuelos que tengan un destino u origen en comun con las ciudades de origen y destino (respectivamente)
-}
destinoUOrigenEnComun :: AgenciaDeViajes -> Ciudad -> Ciudad -> AgenciaDeViajes
destinoUOrigenEnComun [] _ _ = []
destinoUOrigenEnComun ((origen, destino, tiempo) : vuelos) ciudadOrigen ciudadDestino
  | origen == ciudadOrigen = (origen, destino, tiempo) : destinoUOrigenEnComun vuelos ciudadOrigen ciudadDestino
  | destino == ciudadDestino = (origen, destino, tiempo) : destinoUOrigenEnComun vuelos ciudadOrigen ciudadDestino
  | otherwise = destinoUOrigenEnComun vuelos ciudadOrigen ciudadDestino

{-
  Almacena los tiempos entre vuelos con escala o directo en una lista
-}
tiempoDeVueloDirectoOEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
tiempoDeVueloDirectoOEscala [] _ _ = []
tiempoDeVueloDirectoOEscala [(origen, destino, tiempo)] ciudadOrigen ciudadDestino
  | vueloDirecto = [tiempo]
  | otherwise = []
  where
    vueloDirecto = ciudadOrigen == origen && ciudadDestino == destino
tiempoDeVueloDirectoOEscala ((origen1, destino1, tiempo1) : (origen2, destino2, tiempo2) : vuelos) ciudadOrigen ciudadDestino
  | vueloDirecto = tiempo1 : tiempoDeVueloDirectoOEscala ((origen2, destino2, tiempo2) : vuelos) ciudadOrigen ciudadDestino
  | juntasDanConDestino = (tiempo1 + tiempo2) : tiempoDeVueloDirectoOEscala vuelos ciudadOrigen ciudadDestino
  | otherwise = tiempoDeVueloDirectoOEscala ((origen1, destino1, tiempo1) : vuelos) ciudadOrigen ciudadDestino ++ tiempoDeVueloDirectoOEscala ((origen2, destino2, tiempo2) : vuelos) ciudadOrigen ciudadDestino
  where
    vueloDirecto = ciudadOrigen == origen1 && ciudadDestino == destino1
    juntasDanConDestino = origen1 /= origen2 && destino1 /= destino2 && (origen1 == ciudadOrigen || origen2 == ciudadOrigen) && (destino1 == ciudadDestino || destino2 == ciudadDestino) && (destino1 == origen2 || destino2 == origen1)

{-
  Devuelve la minima duración entre vuelos encontrada, sin importar si es directo o con escalas
-}
minimaDuracion :: [Duracion] -> Duracion
minimaDuracion [tiempo] = tiempo
minimaDuracion (tiempo1 : tiempo2 : tiempos)
  | tiempo1 <= tiempo2 = minimaDuracion (tiempo1 : tiempos)
  | otherwise = minimaDuracion (tiempo2 : tiempos)

-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigen vuelos origen = validaMasEscalas [vuelosConMismoOrigen, vuelosDiferentes, vuelosConMismoDestino]
  where
    vuelosConMismoOrigen = conMismoOrigen vuelos origen
    vuelosConMismoDestino = conMismoDestino vuelos origen
    vuelosDiferentes = diferenteACiudad vuelos origen

conMismoOrigen :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
conMismoOrigen [] _ = []
conMismoOrigen ((origen, destino, t) : vuelos) ciudad
  | mismoOrigenYCiudad = (origen, destino, t) : conMismoOrigen vuelos ciudad
  | otherwise = conMismoOrigen vuelos ciudad
  where
    mismoOrigenYCiudad = origen == ciudad

conMismoDestino :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
conMismoDestino [] _ = []
conMismoDestino ((origen, destino, t) : vuelos) ciudad
  | mismoDestinoYCiudad = (origen, destino, t) : conMismoDestino vuelos ciudad
  | otherwise = conMismoDestino vuelos ciudad
  where
    mismoDestinoYCiudad = destino == ciudad

diferenteACiudad :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
diferenteACiudad [] _ = []
diferenteACiudad ((origen, destino, t) : vuelos) ciudad
  | ciudadDiferente = (origen, destino, t) : diferenteACiudad vuelos ciudad
  | otherwise = diferenteACiudad vuelos ciudad
  where
    ciudadDiferente = origen /= ciudad && destino /= ciudad

diferenteA2Ciudades :: AgenciaDeViajes -> Ciudad -> Ciudad -> AgenciaDeViajes
diferenteA2Ciudades [] _ _ = []
diferenteA2Ciudades ((origen, destino, t) : vuelos) ciudad1 ciudad2
  | difDestinoYC1 && difOrigenYC2 = (origen, destino, t) : diferenteA2Ciudades vuelos ciudad1 ciudad2
  | otherwise = diferenteA2Ciudades vuelos ciudad1 ciudad2
  where
    difDestinoYC1 = destino /= ciudad1
    difOrigenYC2 = origen /= ciudad2

validaMasEscalas :: [AgenciaDeViajes] -> Bool
validaMasEscalas ([] : _ : _ : conjunto) = False
validaMasEscalas (_ : _ : [] : conjunto) = False
validaMasEscalas (((origenA, destinoA, tA) : origenIgual) : [] : ((origenB, destinoB, tB) : destinoIgual) : conjunto)
  | destinoA == origenB = True
  | otherwise = validaConSiguienteDestino || validaConSiguienteOrigen
  where
    validaConSiguienteDestino = validaMasEscalas (((origenA, destinoA, tA) : origenIgual) : [] : destinoIgual : conjunto)
    validaConSiguienteOrigen = validaMasEscalas (origenIgual : [] : ((origenB, destinoB, tB) : destinoIgual) : conjunto)
validaMasEscalas (((origenA, destinoA, tA) : origenIgual) : ((origen, destino, t) : diferenteTodo) : ((origenB, destinoB, tB) : destinoIgual) : conjunto)
  | destinoA == origenB = True
  | igualAlDestino && igualAlOrigen = True
  | otherwise = validaConsiguienteDifTodo || validaConSiguienteDestinoIgual || validaconSigueinteOrigenIgual || validaConSiguientesParecidos
  where
    igualAlDestino = destinoA == origen
    igualAlOrigen = origenB == destino
    -- toma el siguiente de diferenteTodo
    validaConsiguienteDifTodo = validaMasEscalas (((origenA, destinoA, tA) : origenIgual) : diferenteTodo : ((origenB, destinoB, tB) : destinoIgual) : conjunto)
    -- toma con el siguiente de la lista destinoIgual
    validaConSiguienteDestinoIgual = validaMasEscalas (((origenA, destinoA, tA) : origenIgual) : ((origen, destino, t) : diferenteTodo) : destinoIgual : conjunto)
    -- toma con el siguiente de la lista origenIgual
    validaconSigueinteOrigenIgual = validaMasEscalas (origenIgual : ((origen, destino, t) : diferenteTodo) : ((origenB, destinoB, tB) : destinoIgual) : conjunto)
    {-
      corrobora con el primero de ambas listas de listas quedando lo siguiente:
        - de (x,_,_) de diferenteTodo / x == destinoA
        - de (x,y,_) de diferenteTodo / y /= destinoA && x /= origenB
        - de (_,y,_) de diferenteTodo / x == origenB
    -}
    -- con esta validación, me aseguro de no recorrer de más para hacer un ida y vuelta al mismo lugar de donde salgo
    validaConSiguientesParecidos = validaMasEscalas [conMismoOrigen ((origen, destino, t) : diferenteTodo) destinoA, diferenteA2Ciudades ((origen, destino, t) : diferenteTodo) destinoA origenB, conMismoDestino ((origen, destino, t) : diferenteTodo) origenB]