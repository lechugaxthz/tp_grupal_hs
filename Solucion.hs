module Solucion where

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)

type AgenciaDeViajes = [Vuelo]

{- EJERCICIO 1
    c(1,2, ... , n) = Ciudad de origen
    d(1,2, ... , n) = Ciudad de destino
    t(1,2, ... , n) =  Duración del vuelo
-}

{-
Explico como funciona el ejercicio 1:
Lo primero que vamos a explicar es la función vueloValido. 
.Esta función nos permite saber si este vuelo tiene origen y destino distinto, y si el tiempo de vuelo es mayor a 0. Si se cumple, tira True. Si no, tira False
vuelosValidos tiene dos casos base: Si la lista está vacia o si tiene un elemento. 
1) si la lista está vacía, tira False
2) Si la lista tiene un elemento, comprueba VueloValido en ese elemento, y tira True o False. 
Si tiene mas de un elemento, lo piensa así.
1) Los dos primeros vuelos deben ser válidos
2) El origen 1 debe ser distinto del origen 2, lo mismo para los destinos. 
3) La recursión es realizada dos veces: Una vez sobre la lista quitandole el elemento 2, y otra vez sobre la lista quitandole el elemento 1.
3b) Esto se va a ir repitiendo hasta que quede un elemento, en donde pasa vueloValido sobre este elemento, y decide si True o False 
4) Esto me va a dar una cadena de booleanos, en donde la única manera de que funcione bien esto es que todos sean verdaderos ;)
-}
vuelosValidos :: AgenciaDeViajes -> Bool 
vuelosValidos [] = False
vuelosValidos [x] = vueloValido x 
vuelosValidos ((c1,d1,t1):(c2,d2,t2):xs) = vueloValido1 && vueloValido2 && (c1 /= c2 || d1 /= d2) && vuelosValidos ((c1,d1,t1) : xs) && vuelosValidos ((c2,d2,t2) : xs)
    where
        vueloValido1 = vueloValido (c1,d1,t1)
        vueloValido2 = vueloValido (c2,d2,t2)


--vueloValido va a verificar que el origen y el destino sean distintos, y que el tiempo de viaje sea mayor a 0
vueloValido :: Vuelo -> Bool
vueloValido (c, d, t) = c /= d && t > 0


-- EJERCICIO 2
ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas [] _ = [] -- Caso base 


-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota _ = [("BsAs","Rosario",9.0)] -- Borrar y escribir el código correcto


-- EJERCICIO 4

ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada (x:xs) = tuplaConMasApariciones (agenciaATupla (x:xs))

--tuplaConMasApariciones procesa una lista compuesta por tuplas (Ciudad,Apariciones), y nos da la tupla con mas apariciones
tuplaConMasApariciones :: [(Ciudad, Integer)] -> Ciudad
tuplaConMasApariciones [(c,a)] = c 
tuplaConMasApariciones ((c1,a1):(c2,a2):xs) | a1 >= a2 = tuplaConMasApariciones ((c1,a1):xs)
                                            | otherwise = tuplaConMasApariciones ((c2,a2):xs)
--agenciaATupla agarra una lista del formato AgenciaDeViajes, y envía esta lista para conversorString, la cual puede procesar generarTupla
agenciaATupla :: AgenciaDeViajes -> [(Ciudad, Integer)]
agenciaATupla ((c,d,t):xs) = generarTupla (conversorString ((c,d,t):xs))

--generarTupla agarra una lista del formato [String], y genera tuplas con el nombre del string y la cantidad de veces (Apariciones) que está en la lista
generarTupla :: [String] -> [(Ciudad, Integer)]
generarTupla [] = []
generarTupla (x:xs) = (x,cantidadDeApariciones x (x:xs)) : generarTupla (quitarElementos x xs)

--conversorString envía los orígenes y destinos de nuestra agencia de viajes a una lista aparte, para mejor manejo. 
conversorString :: AgenciaDeViajes -> [String]
conversorString [] = []
conversorString ((c,d,t):xs) = [c,d] ++ conversorString xs

--Cuenta la cantidad de veces que aparece el string que nos interesa en la lista
cantidadDeApariciones :: String -> [String] -> Integer
cantidadDeApariciones ciudad [] = 0
cantidadDeApariciones ciudad (x:xs) | ciudad == x = 1 + cantidadDeApariciones ciudad xs
                                    | otherwise = cantidadDeApariciones ciudad xs

--cantidadDeApariciones, pero metiendole directamente AgenciaDeViajes (sirve para otros ejercicios)
cantidadDeAparicionesTuplas :: String -> AgenciaDeViajes -> Integer
cantidadDeAparicionesTuplas ciudad agencia = cantidadDeApariciones ciudad (conversorString agencia)

--Va quitando los elementos que queramos de la lista
quitarElementos :: String -> [String] -> [String]
quitarElementos nombre [] = []
quitarElementos nombre (x:xs) | nombre == x = quitarElementos nombre xs 
                              | otherwise = x : quitarElementos nombre xs





-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar vuelos origen destino = True -- Borrar y escribir el código correcto


-- EJERCICIO 6
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido _ _ _ = 10.0 -- Borrar y escribir el código correcto



-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen vuelos origen = True -- Borrar y escribir el código correcto