import Solucion
import Test.HUnit

-- test ejercicio 3
agenciaEj3 = [
    ("Buenos Aires", "Cordoba", 1.5),
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
testModernizaFlota = test [
    "Flota modernizada -> True" ~: funcionTest3 agenciaEj3 ~?= True,
    "Flota vacía -> True -> No hay flotas a modernizar" ~: funcionTest3 agenciaEj3 ~?= True
  ]

-- ahorro de espacio con funciones con nombre mas chico
funcionTest3 x = cadaValorEsModernizado (modernizarFlota x) (funcionPrueba x) x
-- fin

-- aux para test
frst :: Vuelo -> Ciudad
frst (x,_,_) = x

scnd :: Vuelo -> Ciudad
scnd (_,y,_) = y

thd :: Vuelo -> Duracion
thd (_,_,z) = z

funcionPrueba :: AgenciaDeViajes -> AgenciaDeViajes
funcionPrueba flota = [( frst (flota !! x),scnd (flota !! x), (thd (flota !! x))* 0.9) | x <- [0.. length flota -1]]

cadaValorEsModernizado :: AgenciaDeViajes -> AgenciaDeViajes -> AgenciaDeViajes -> Bool
cadaValorEsModernizado [] [] [] = True
cadaValorEsModernizado ((_,_,t1): viajesFuncSol) ((_,_,t2): viajesFuncPrueba) ((_,_,t3): viajesOriginales)
  | entreFuncValenIgual = True && cadaValorEsModernizado viajesFuncSol viajesFuncPrueba viajesOriginales
  | otherwise = False
  where 
    entreFuncValenIgual = t1 == t2 && t1 < t3 && t3 * 0.9 == t1


-- test ejercicio 7
agencia = [("Buenos Aires", "Cordoba", 1), ("Cordoba", "Santa Fe", 1), ("Santa Fe", "Neuquen", 1), ("Neuquen", "Misiones", 1), ("Misiones", "Jujuy", 1), ("Jujuy", "Corrientes", 1), ("Corrientes", "Santiago Del Estero", 1), ("Santiago Del Estero", "Chaco", 1), ("Chaco", "Chubut", 1), ("Chubut", "La Pampa", 1), ("La Pampa", "Mendoza", 1), ("Mendoza", "San Juan", 1), ("San Juan", "Santa Cruz", 1), ("Santa Cruz", "Tucuman", 1), ("Tucuman", "Rio Negro", 1), ("Rio Negro", "Formosa", 1), ("Formosa", "Entre Rios", 1), ("Entre Rios", "Catamarca", 1), ("Catamarca", "La Rioja", 1), ("La Rioja", "Buenos Aires", 1)]
agencia2 = [("Cordoba","Santa Fe",1),("Santa Fe","Cordoba",1),("Misiones","Cordoba",1),("Buenos Aires","Cordoba",1),("Buenos Aires","Neuquen",1),("Neuquen","Santa Fe",1),("Neuquen","Santa Fe",1),("Chubut","Buenos Aires",1),("Neuquen","Buenos Aires",1),("Buenos Aires","Santa Fe",1),("Misiones","Buenos Aires",1)]
agenciaSinConexion = [("Buenos Aires", "Cordoba", 1), ("Cordoba", "Santa Fe", 1), ("Santa Fe", "Neuquen", 1), ("Neuquen", "Misiones", 1), ("Jujuy", "Corrientes", 1), ("Corrientes", "Santiago Del Estero", 1), ("Santiago Del Estero", "Chaco", 1), ("Chaco", "Chubut", 1), ("Chubut", "La Pampa", 1), ("La Pampa", "Mendoza", 1), ("Mendoza", "San Juan", 1), ("San Juan", "Santa Cruz", 1), ("Santa Cruz", "Tucuman", 1), ("Tucuman", "Rio Negro", 1), ("Rio Negro", "Formosa", 1), ("Formosa", "Entre Rios", 1), ("Entre Rios", "Catamarca", 1), ("Catamarca", "La Rioja", 1), ("La Rioja", "Buenos Aires", 1)]

testPuedoVolverAOrigen :: Test
testPuedoVolverAOrigen =
  test
    [
      "valor verdadero" ~: puedoVolverAOrigen agencia "Buenos Aires" ~?= True,
      "valor verdadero" ~: puedoVolverAOrigen agencia2 "Buenos Aires" ~?= True,
      "valor falso" ~: puedoVolverAOrigen agenciaSinConexion "Buenos Aires" ~?= False
    ]

testFiltradoPorOrigenYODestino :: Test
testFiltradoPorOrigenYODestino =
  test 
    [
      "con mismo origen -> origen Buenos Aires -> lista True" ~: length (conMismoOrigen agencia "Buenos Aires") ~?= 1
    ]


allTest = test [
  "test función principal" ~: testPuedoVolverAOrigen,
  "test función secundaria filtrados" ~: testFiltradoPorOrigenYODestino
  ]

runTest = runTestTT allTest