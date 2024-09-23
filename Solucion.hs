
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


--vueloValido va a verificar que el origen y el destino sean distintos, y que el tiempo de viaje sea mayor a 0
vueloValido :: Vuelo -> Bool
vueloValido (c, d, t) = c /= d && t > 0


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

pertenece1 :: Ciudad -> [Ciudad] -> Bool
pertenece1 _ [] = False
pertenece1 x (y:ys) = x == y || pertenece1 x ys

sacarCiudadEspecifica :: Ciudad -> [Ciudad] -> [Ciudad]
sacarCiudadEspecifica _ [] = []
sacarCiudadEspecifica x (y:ys)
    | x == y = sacarCiudadEspecifica x ys
    | otherwise = y : (sacarCiudadEspecifica x ys)

-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota _ = [("BsAs","Rosario",9.0)] -- Borrar y escribir el código correcto


-- EJERCICIO 4

ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada [x] = x
ciudadMasConectada ((c1,d1,t1):(c2,d2,t2):xs) | (numConexiones C1 >= numConexiones C2 && numConexiones C1 >= numConexiones D2) || (numConexionesD1 >= numConexionesD2 && numConexiones D1 >= numConexiones C2) = ciudadMasConectada ((c1,d1,t1):xs)
                                              | otherwise = ciudadMasConectada ((c2,d2,t2):xs)
    where
        numConexionesC1  = longitud (ciudadesConectadas ((c1,d1,t1):(c2,d2,t2):xs) c1)
        numConexionesC2  = longitud (ciudadesConectadas ((c1,d1,t1):(c2,d2,t2):xs) c2)
        numConexionesD1 = longitud (ciudadesConectadas ((c1,d1,t1):(c2,d2,t2):xs) d1)
        numConexionesD2 = longitud (ciudadesConectadas ((c1,d1,t1):(c2,d2,t2):xs) d2)

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar vuelos origen destino = True -- Borrar y escribir el código correcto


-- EJERCICIO 6
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido _ _ _ = 10.0 -- Borrar y escribir el código correcto



-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen vuelos origen = True -- Borrar y escribir el código correcto