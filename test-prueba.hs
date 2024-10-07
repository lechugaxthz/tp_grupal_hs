import Solucion

import Test.HUnit

-- test ejercicio 3
agenciaEj3 :: AgenciaDeViajes
agenciaEj3 =
  [ ("Buenos Aires", "Cordoba", 1.5),
    ("Cordoba", "Mendoza", 1.2),
    ("Mendoza", "Salta", 1.8),
    ("Salta", "Tucuman", 0.5),
    ("Tucuman", "Santiago del Estero", 0.6),
    ("Santiago del Estero", "Santa Fe", 0.7),
    ("Santa Fe", "La Pampa", 1.1),
    ("La Pampa", "Neuquen", 1.4),
    ("Neuquen", "Rio Negro", 1.0),
    ("Rio Negro", "Chubut", 1.9)
  ]

testModernizaFlota :: Test
testModernizaFlota =
  test
    [ "Flota modernizada -> True" ~: funcionTest3 agenciaEj3 ~?= True,
      "Flota vacía -> True -> No hay flotas a modernizar" ~: funcionTest3 [] ~?= True
    ]

-- ahorro de espacio con funciones con nombre mas chico
funcionTest3 :: AgenciaDeViajes -> Bool
funcionTest3 x = cadaValorEsModernizado (modernizarFlota x) x

-- fin

-- aux para test
frst :: Vuelo -> Ciudad
frst (x, _, _) = x

scnd :: Vuelo -> Ciudad
scnd (_, y, _) = y

thd :: Vuelo -> Duracion
thd (_, _, z) = z

cadaValorEsModernizado :: AgenciaDeViajes -> AgenciaDeViajes -> Bool
cadaValorEsModernizado [] [] = True
cadaValorEsModernizado ((_, _, t1) : viajesFuncSol) ((_, _, t3) : viajesOriginales)
  | entreFuncValenIgual = cadaValorEsModernizado viajesFuncSol viajesOriginales
  | otherwise = False
  where
    entreFuncValenIgual = t1 < t3 && t3 * 0.9 == t1

-- test ejercicio 6

agenciaEj4 :: AgenciaDeViajes
agenciaEj4 = agenciaEj3

-- test fn principal
testDuraciónDelCaminoMasRapido :: Test
testDuraciónDelCaminoMasRapido =
  test
    [ "Vuelo directo -> True -> duración del vuelo" ~: funcionTest4 True agenciaEj4 "La Pampa" "Neuquen" ~?= True,
      "Vuelo con escala -> True -> duración de ambos vuelos" ~: funcionTest4 False agenciaEj4 "Cordoba" "Salta" ~?= True
    ]

-- ahorro de espacio con funciones con nombre mas chico
funcionTest4 :: Bool -> AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
funcionTest4 simple viajes origen destino 
  | simple = duracionDelCaminoMasRapido viajes origen destino == vueloDirecto viajes origen destino
  | otherwise = duracionDelCaminoMasRapido viajes origen destino == minimaDuracion listaDeTiemposEnEscala
    where
      conOrigenIgual = conMismoOrigen viajes origen
      conDestinoIgual = conMismoDestino viajes destino
      listaDeTiemposEnEscala = vueloConEscala conOrigenIgual conDestinoIgual


vueloDirecto :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
vueloDirecto ((origen, destino, t): vuelos) cdOrigen cdDestino
  | origen == cdOrigen && destino == cdDestino = t
  | otherwise = vueloDirecto vuelos cdOrigen cdDestino

vueloConEscala :: AgenciaDeViajes -> AgenciaDeViajes -> [Duracion]
vueloConEscala [] _ = []
vueloConEscala _ [] = []
vueloConEscala ((origen1, destino1, t1):viajes1) ((origen2, destino2, t2):viajes2)
  | destinoYOrigen = t1 + t2 : vueloConEscala viajes1 viajes2
  | otherwise = vueloConEscala viajes1 ((origen2, destino2, t2):viajes2) ++ vueloConEscala ((origen1, destino1,t1): viajes1) viajes2
  where 
    destinoYOrigen = destino1 == origen2


-- test ejercicio 7
agencia :: AgenciaDeViajes
agencia = [("Buenos Aires", "Cordoba", 1), ("Cordoba", "Santa Fe", 1), ("Santa Fe", "Neuquen", 1), ("Neuquen", "Misiones", 1), ("Misiones", "Jujuy", 1), ("Jujuy", "Corrientes", 1), ("Corrientes", "Santiago Del Estero", 1), ("Santiago Del Estero", "Chaco", 1), ("Chaco", "Chubut", 1), ("Chubut", "La Pampa", 1), ("La Pampa", "Mendoza", 1), ("Mendoza", "San Juan", 1), ("San Juan", "Santa Cruz", 1), ("Santa Cruz", "Tucuman", 1), ("Tucuman", "Rio Negro", 1), ("Rio Negro", "Formosa", 1), ("Formosa", "Entre Rios", 1), ("Entre Rios", "Catamarca", 1), ("Catamarca", "La Rioja", 1), ("La Rioja", "Buenos Aires", 1)]

agencia2 :: AgenciaDeViajes
agencia2 = [("Cordoba", "Santa Fe", 1), ("Santa Fe", "Cordoba", 1), ("Misiones", "Cordoba", 1), ("Buenos Aires", "Cordoba", 1), ("Buenos Aires", "Neuquen", 1), ("Neuquen", "Santa Fe", 1), ("Neuquen", "Santa Fe", 1), ("Chubut", "Buenos Aires", 1), ("Neuquen", "Buenos Aires", 1), ("Buenos Aires", "Santa Fe", 1), ("Misiones", "Buenos Aires", 1)]

agenciaSinConexion :: AgenciaDeViajes
agenciaSinConexion = [("Buenos Aires", "Cordoba", 1), ("Cordoba", "Santa Fe", 1), ("Santa Fe", "Neuquen", 1), ("Neuquen", "Misiones", 1), ("Jujuy", "Corrientes", 1), ("Corrientes", "Santiago Del Estero", 1), ("Santiago Del Estero", "Chaco", 1), ("Chaco", "Chubut", 1), ("Chubut", "La Pampa", 1), ("La Pampa", "Mendoza", 1), ("Mendoza", "San Juan", 1), ("San Juan", "Santa Cruz", 1), ("Santa Cruz", "Tucuman", 1), ("Tucuman", "Rio Negro", 1), ("Rio Negro", "Formosa", 1), ("Formosa", "Entre Rios", 1), ("Entre Rios", "Catamarca", 1), ("Catamarca", "La Rioja", 1), ("La Rioja", "Buenos Aires", 1)]

testPuedoVolverAOrigen :: Test
testPuedoVolverAOrigen =
  test
    [ "valor verdadero" ~: puedoVolverAOrigen agencia "Buenos Aires" ~?= True,
      "valor verdadero" ~: puedoVolverAOrigen agencia2 "Buenos Aires" ~?= True,
      "valor falso" ~: puedoVolverAOrigen agenciaSinConexion "Buenos Aires" ~?= False
    ]

testFiltradoPorOrigenYODestino :: Test
testFiltradoPorOrigenYODestino =
  test
    [ "con mismo origen -> origen Buenos Aires -> lista True" ~: length (conMismoOrigen agencia "Buenos Aires") ~?= 1
    ]

allTest :: Test
allTest =
  test
    [ "test función principal" ~: testPuedoVolverAOrigen,
      "test función secundaria filtrados" ~: testFiltradoPorOrigenYODestino
    ]

runTest :: IO Counts
runTest = runTestTT allTest

{-
vueloConUnaOMasEscalas :: AgenciaDeViajes -> AgenciaDeViajes -> Ciudad -> Ciudad -> AgenciaDeViajes
vueloConUnaOMasEscalas [] _ _ = []
vueloConUnaOMasEscalas (vuelo : vuelos) vuelosDif ciudadOrigen ciudadDestino
  | 
  where
    esMismoVuelo = frst vuelo == ciudadOrigen && scnd vuelo == ciudadDestino
    vueloADestino = validaMasEscalas [[vuelo], vuelosDif, ]
    -}