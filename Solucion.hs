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
puedoVolverAOrigen vuelos origen = True -- Borrar y escribir el código correcto
