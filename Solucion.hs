
type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)

type AgenciaDeViajes = [Vuelo]

{- EJERCICIO 1
    c(1,2, ... , n) = Ciudad de origen
    d(1,2, ... , n) = Ciudad de destino
    t(1,2, ... , n) =  Duración del vuelo
-}

vuelosValidos :: AgenciaDeViajes -> Bool 
vuelosValidos [] = False
vuelosValidos [x] = True
vuelosValidos ((c1,d1,t1):(c2,d2,t2):xs) = vueloValido1 && vueloValido2 && (c1 /= c2 || d1 /= d2) && vuelosValidos ((c1,d1,t1) : xs) && vuelosValidos ((c2,d2,t2) : xs)
    where
        vueloValido1 = vueloValido (c1,d1,t1)
        vueloValido2 = vueloValido (c2,d2,t2)

--EJEMPLO VUELOS TODOS DISTINTOS (de 4) = [("Rosario", "Cordoba", 3), ("Cordoba", "Rosario", 3), ("Tucuman", "Rosario", 4.4), ("San Luis", "Rosario", 6)]
--EJEMPLO VUELOS CON REPETIDOS (de 4) =  [("Rosario", "Cordoba", 3), ("Cordoba", "Rosario", 3), ("Rosario", "Cordoba", 3)]

vueloValido :: Vuelo -> Bool
vueloValido (c1, c2, t) = c1 /= c2 && t > 0


-- EJERCICIO 2
ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas _ _ = ["BsAs"] -- Borrar y escribir el código correcto

-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota _ = [("BsAs","Rosario",9.0)] -- Borrar y escribir el código correcto


-- EJERCICIO 4
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada _ = "Rosario" -- Borrar y escribir el código correcto


-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar vuelos origen destino = True -- Borrar y escribir el código correcto


-- EJERCICIO 6
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido _ _ _ = 10.0 -- Borrar y escribir el código correcto



-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen vuelos origen = True -- Borrar y escribir el código correcto