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
ciudadesConectadas _ _ = ["BsAs"] -- Borrar y escribir el código correcto

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
sePuedeLlegar vuelos origen destino = True -- Borrar y escribir el código correcto

-- EJERCICIO 6
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido viajes ciudadOrigen ciudadDestino = minimaDuracion (tiempoDeVueloDirectoOEscala (destinoUOrigenEnComun viajes ciudadOrigen ciudadDestino) ciudadOrigen ciudadDestino)

destinoUOrigenEnComun :: AgenciaDeViajes -> Ciudad -> Ciudad -> AgenciaDeViajes
destinoUOrigenEnComun [] _ _ = []
destinoUOrigenEnComun ((origen, destino, tiempo) : vuelos) ciudadOrigen ciudadDestino
  | origen == ciudadOrigen = (origen, destino, tiempo) : destinoUOrigenEnComun vuelos ciudadOrigen ciudadDestino
  | destino == ciudadDestino = (origen, destino, tiempo) : destinoUOrigenEnComun vuelos ciudadOrigen ciudadDestino
  | otherwise = destinoUOrigenEnComun vuelos ciudadOrigen ciudadDestino

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

minimaDuracion :: [Duracion] -> Duracion
minimaDuracion [tiempo] = tiempo
minimaDuracion (tiempo1 : tiempo2 : tiempos)
  | tiempo1 <= tiempo2 = minimaDuracion (tiempo1 : tiempos)
  | otherwise = minimaDuracion (tiempo2 : tiempos)

-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigen vuelos origen = validaMasEscalas2 [conMismoOrigen vuelos origen, diferenteACiudad vuelos origen, conMismoDestino vuelos origen]


conMismoOrigen :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
conMismoOrigen [] _ = []
conMismoOrigen ((origen, destino, t) : viajes) ciudad
  | mismoOrigenYCiudad = (origen, destino, t) : conMismoOrigen viajes ciudad
  | otherwise = conMismoOrigen viajes ciudad
  where
    mismoOrigenYCiudad = origen == ciudad

conMismoDestino :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
conMismoDestino [] _ = []
conMismoDestino ((origen, destino, t) : viajes) ciudad
  | mismoDestinoYCiudad = (origen, destino, t) : conMismoDestino viajes ciudad
  | otherwise = conMismoDestino viajes ciudad
  where
    mismoDestinoYCiudad = destino == ciudad

diferenteACiudad :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
diferenteACiudad [] _ = []
diferenteACiudad ((origen, destino, t) : viajes) ciudad
  | ciudadDiferente = (origen, destino, t) : diferenteACiudad viajes ciudad
  | otherwise = diferenteACiudad viajes ciudad
  where
    ciudadDiferente = origen /= ciudad && destino /= ciudad

diferenteA2Ciudades :: AgenciaDeViajes -> Ciudad -> Ciudad -> AgenciaDeViajes
diferenteA2Ciudades [] _  _ = []
diferenteA2Ciudades ((origen, destino, t) : viajes) ciudad1 ciudad2
  | difDestinoYC1 && difOrigenYC2 = (origen, destino, t) : diferenteA2Ciudades viajes ciudad1 ciudad2
  | otherwise = diferenteA2Ciudades viajes ciudad1 ciudad2
  where
    difDestinoYC1 = destino /= ciudad1
    difOrigenYC2 = origen /= ciudad2


validaMasEscalas2 :: [AgenciaDeViajes] -> Bool
validaMasEscalas2 ([]:_:_: conjunto) = False
validaMasEscalas2 (_:_:[]: conjunto) = False
validaMasEscalas2 (((origenA , destinoA, tA) : origenIgual) : [] : ((origenB, destinoB, tB) : destinoIgual): conjunto)
  | destinoA == origenB = True
  | otherwise = validaConSiguienteDestino || validaConSiguienteOrigen
  where
    validaConSiguienteDestino = validaMasEscalas2 (((origenA , destinoA, tA) : origenIgual) : [] : destinoIgual: conjunto)
    validaConSiguienteOrigen = validaMasEscalas2 (origenIgual : [] : ((origenB, destinoB, tB) : destinoIgual): conjunto)

validaMasEscalas2 (((origenA , destinoA, tA) : origenIgual) : ((origen, destino, t): diferenteTodo) : ((origenB, destinoB, tB) : destinoIgual) : conjunto) 
  | destinoA == origenB = True
  | igualAlDestino && igualAlOrigen = True
  | otherwise = validaConSiguienteDestinoDif || validaConSiguienteDestinoIgual || validaconSigueinteOrigenIgual || validaConSiguientesParecidos
  where 
    igualAlDestino = destinoA == origen
    igualAlOrigen = origenB == destino
    validaConSiguienteDestinoDif = validaMasEscalas2 (((origenA,destinoA,tA): origenIgual) : diferenteTodo : ((origenB,destinoB,tB):destinoIgual) : conjunto)
    validaConSiguienteDestinoIgual = validaMasEscalas2 (((origenA,destinoA,tA): origenIgual) : ((origen, destino,t):diferenteTodo) : destinoIgual : conjunto)
    validaconSigueinteOrigenIgual = validaMasEscalas2 ( origenIgual : ((origen, destino,t): diferenteTodo) : ((origenB, destinoB, tB) : destinoIgual) : conjunto) 
    validaConSiguientesParecidos = validaMasEscalas2 [conMismoOrigen ((origen, destino,t): diferenteTodo) destinoA, diferenteA2Ciudades ((origen, destino,t): diferenteTodo) destinoA origenB, conMismoDestino ((origen, destino,t): diferenteTodo) origenB]

