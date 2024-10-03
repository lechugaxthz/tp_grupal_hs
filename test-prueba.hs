import Solucion
import Test.HUnit

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