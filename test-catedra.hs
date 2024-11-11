import Data.List
import Solucion
import Test.HUnit

-- No está permitido agregar nuevos imports.

runCatedraTests :: IO Counts
runCatedraTests = runTestTT allTests

allTests :: Test
allTests =
  test
    [ "vuelosValidos"
        ~: testsEjvuelosValidos,
      "ciudadesConectadas"
        ~: testsEjciudadesConectadas,
      "modernizarFlota"
        ~: testsEjmodernizarFlota,
      "ciudadMasConectada"
        ~: testsEjciudadMasConectada,
      "sePuedeLlegar"
        ~: testsEjsePuedeLlegar,
      "duracionDelCaminoMasRapido"
        ~: testsEjduracionDelCaminoMasRapido,
      "puedoVolverAOrigen"
        ~: testsEjpuedoVolverAOrigen
    ]

-- corregir los tests si es necesario con las funciones extras que se encuentran al final del archivo

-- test ejercicio 1
testsEjvuelosValidos :: Test
testsEjvuelosValidos =
  test
    [ "no hay vuelos"
        ~: vuelosValidos []
        ~?= True,
      "vuelos válidos con 1 elemento"
        ~: vuelosValidos [("BsAs", "Rosario", 5.0)]
        ~?= True,
      "vuelos no válidos con 1 elemento, tiempo de vuelo 0"
        ~: vuelosValidos [("BsAs", "Rosario", 0)]
        ~?= False,
      "vuelos no válidos con 1 elemento, ciudades repetidas"
        ~: vuelosValidos [("Rosario", "Rosario", 5)]
        ~?= False,
      "vuelos válidos con 2 elementos distintos"
        ~: vuelosValidos [("Rosario", "Cordoba", 4), ("Tucuman", "Cordoba", 3)]
        ~?= True,
      "vuelos válidoss con 2 elementos, mismo viaje ida y vuelta"
        ~: vuelosValidos [("BsAs", "Cordoba", 4), ("Cordoba", "Rosario", 4)]
        ~?= True,
      "vuelos no válidos con 2 elementos, mismo viaje repetido"
        ~: vuelosValidos [("BsAs", "Tucuman", 5), ("BsAs", "Tucuman", 5)]
        ~?= False,
      "vuelos no válidos con 2 elementos, mismo viaje con distinto tiempo"
        ~: vuelosValidos [("Tucuman", "Salta", 2), ("Tucuman", "Salta", 3)]
        ~?= False,
      "vuelos válidos con 3 elementos,elementos distintos"
        ~: vuelosValidos [("BsAs", "Tucuman", 5), ("Jujuy", "BsAs", 4), ("Salta", "Resistencia", 3)]
        ~?= True,
      "vuelos válidos con 3 elementos, dos ida y vuelta y un distinto"
        ~: vuelosValidos [("Tucuman", "BsAs", 4), ("BsAs", "Tucuman", 4), ("Montevideo", "Rosario", 3)]
        ~?= True,
      "vuelos válidos con 3 elementos, dos ida y vuelta y un distinto CASO DOS"
        ~: vuelosValidos [("Rosario", "Damasco", 4), ("Montevideo", "Rosario", 3), ("Damasco", "Rosario", 4)]
        ~?= True,
      "vuelos no válidos con 3 elementos, todos iguales"
        ~: vuelosValidos [("Rosario", "Tucuman", 4), ("Rosario", "Tucuman", 4), ("Rosario", "Tucuman", 4)]
        ~?= False,
      "vuelos no válidos con 3 elementos, dos iguales y uno distinto"
        ~: vuelosValidos [("Rosario", "Tucuman", 1), ("Tucuman", "Cordoba", 2), ("Rosario", "Tucuman", 1)]
        ~?= False,
      "vuelos no válidos con 3 elementos, dos iguales con distinto tiempo"
        ~: vuelosValidos [("Montevideo", "BsAs", 4), ("Montevideo", "BsAs", 5), ("Rosario", "Santa Fe", 1)]
        ~?= False,
      "vuelos no válidos con 3 elementos, uno con tiempo de vuelo 0"
        ~: vuelosValidos [("Montevideo", "BsAs", 1), ("Tucuman", "Posadas", 2), ("Tucuman", "Salta", 0)]
        ~?= False,
      "vuelos no válidos con 3 elementos, todos con tiempo de vuelo 0"
        ~: vuelosValidos [("Nueva York", "Rosario", 0), ("Tokyo", "Formosa", 0), ("Pekin", "Ushuaia", 0)]
        ~?= False,
      "vuelos no válidos con 3 elementos, dos con tiempo de vuelo 0"
        ~: vuelosValidos [("Rosario", "Damasco", 1), ("Tucuman", "Cordoba", 0), ("Damasco", "Tucuman", 0)]
        ~?= False,
      "vuelos no válidos con 3 elementos, dos con tiempo de vuelo 0 CASO DOS"
        ~: vuelosValidos [("Rosario", "Damascco", 0), ("Tokyo", "Formosa", 4), ("Berlin", "Rawson", 0)]
        ~?= False,
      "vuelos válidos con 4 elementos, todos distintos"
        ~: vuelosValidos [("Tucuman", "La Plata", 3), ("Rosario", "Tokyo", 4), ("Tokyo", "Nueva York", 2), ("Helsinki", "Barcelona", 2)]
        ~?= True,
      "vuelos válidos con 4 elementos, dos idas y dos vueltas"
        ~: vuelosValidos [("Tucuman", "Helsinki", 2), ("Rosario", "Rawson", 3), ("Helsinki", "Tucuman", 2), ("Rawson", "Rosario", 3)]
        ~?= True,
      "vuelos válidos con 4 elementos, dos idas y dos vueltas CASO DOS"
        ~: vuelosValidos [("Helsinki", "Teheran", 2), ("Rawson", "Tucuman", 4), ("Tucuman", "Rawson", 4), ("Teheran", "Helsinki", 2)]
        ~?= True,
      "vuelos válidos con 4 elementos, dos idas y dos vueltas CASO TRES"
        ~: vuelosValidos [("Helsinki", "Tucuman", 2), ("Tucuman", "Helsinki", 2), ("Rosario", "Catamarca", 3), ("Catamarca", "Rosario", 3)]
        ~?= True,
      "vuelos no válidos con 4 elementos, todos iguales"
        ~: vuelosValidos [("Ciudad1", "Ciudad2", 4), ("Ciudad1", "Ciudad2", 4), ("Ciudad1", "Ciudad2", 4), ("Ciudad1", "Ciudad2", 4)]
        ~?= False,
      "vuelos no válidos con 4 elementos, dos iguales dos distintos"
        ~: vuelosValidos [("Ciudad1", "Ciudad2", 3), ("Ciudad3", "Ciudad4", 5), ("Ciudad1", "Ciudad2", 3), ("Ciudad3", "Ciudad4", 5)]
        ~?= False,
      "vuelos no válidos con 4 elementos, dos iguales dos distintos con distinto tiempo"
        ~: vuelosValidos [("Ciudad1", "Ciudad2", 3), ("Ciudad3", "Ciudad4", 5), ("Ciudad1", "Ciudad2", 4), ("Ciudad3", "Ciudad4", 6)]
        ~?= False,
      "vuelo valido aux"
        ~: testsEjvueloValido
    ]

testsEjvueloValido :: Test
testsEjvueloValido =
  test
    [ "Tupla con dos ciudades distintas y t>0"
        ~: vueloValido ("Rosario", "Cordoba", 3)
        ~?= True,
      "Tupla con dos ciudades iguales y t>0"
        ~: vueloValido ("Rosario", "Rosario", 2)
        ~?= False,
      "Tupla con dos ciudades distintas y t < 0"
        ~: vueloValido ("Tucuman", "Rosario", 0)
        ~?= False,
      "Tupla con dos ciudades distintas y t < 0 CASO DOS"
        ~: vueloValido ("Rosario", "Cordoba", -2)
        ~?= False,
      "Tupla con dos ciudades iguales y t < 0"
        ~: vueloValido ("Rosario", "Rosario", 0)
        ~?= False,
      "Tupla con dos ciudades iguales y t < 0 CASO DOS"
        ~: vueloValido ("Rosario", "Rosario", -4)
        ~?= False
    ]

-- test ejercicio 2
testsEjciudadesConectadas =
  test
    [ "agencia vacia"
        ~: ciudadesConectadas [] "Rosario"
        ~?= [],
      "ciudad conectada con un elemento"
        ~: ciudadesConectadas [("BsAs", "Rosario", 5.0)] "Rosario"
        ~?= ["BsAs"],
      "Agencia de viajes tiene 1 elemento, ciudad no está"
        ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5)] "Rosario") [],
      "Agencia de viajes tiene 1 elemento, ciudad es origen"
        ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5)] "Buenos Aires") ["Cordoba"],
      "Agencia de viajes tiene 1 elemento, ciudad es destino"
        ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5)] "Cordoba") ["Buenos Aires"],
      "Agencia de viajes varios elementos, ciudad no está"
        ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5), ("Cordoba", "Mendoza", 1.2), ("Mendoza", "Salta", 1.8), ("Salta", "Tucuman", 0.5)] "Chubut") [],
      "y ciudad está 1 vez como origen, su destino no se repite en otro vuelo"
        ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5), ("Chubut", "Mendoza", 1.2), ("Mendoza", "Salta", 1.8), ("Salta", "Tucuman", 0.5)] "Buenos Aires") ["Cordoba"],
      "y ciudad está 1 vez como origen, su destino se repite en otro vuelo"
        ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5), ("Cordoba", "Mendoza", 1.2), ("Mendoza", "Salta", 1.8), ("Salta", "Tucuman", 0.5)] "Buenos Aires") ["Cordoba"],
      "y ciudad está 1 vez como destino, su origen no se repite en otro vuelo"
        ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5), ("Chubut", "Mendoza", 1.2), ("Tucuman", "Salta", 1.8), ("Salta", "Tucuman", 0.5)] "Mendoza") ["Chubut"],
      "y ciudad está 1 vez como destino, su origen se repite en otro vuelo"
        ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5), ("Chubut", "Mendoza", 1.2), ("Tucuman", "Chubut", 1.8), ("Salta", "Tucuman", 0.5)] "Mendoza") ["Chubut"],
      "y ciudad está más de una vez, solo como origen, y destinos no se repiten"
        ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5), ("Buenos Aires", "Mendoza", 1.2), ("Tucuman", "Chubut", 1.8), ("Salta", "Tucuman", 0.5)] "Buenos Aires") ["Cordoba", "Mendoza"],
      "y ciudad está más de una vez, solo como origen, y sus destinos se repiten"
        ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5), ("Buenos Aires", "Mendoza", 1.2), ("Tucuman", "Chubut", 1.8), ("Cordoba", "Mendoza", 0.5)] "Buenos Aires") ["Cordoba", "Mendoza"],
      "y ciudad está más de una vez, solo como destino, y sus origenes no se repiten"
        ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5), ("Buenos Aires", "Mendoza", 1.2), ("Tucuman", "Cordoba", 1.8), ("Salta", "Jujuy", 0.5)] "Cordoba") ["Santa Fe", "Tucuman"],
      "y ciudad está más de una vez, solo como destino, y sus origenes se repiten"
        ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5), ("Buenos Aires", "Santa Fe", 1.2), ("Tucuman", "Cordoba", 1.8), ("Tucuman", "Jujuy", 0.5)] "Cordoba") ["Santa Fe", "Tucuman"],
      "y ciudad está más de una vez, mezcla de origen y destino, y sus destinos/origenes no se repiten"
        ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5), ("Cordoba", "Mendoza", 1.2), ("Cordoba", "Chubut", 1.8), ("Salta", "Jujuy", 0.5)] "Cordoba") ["Santa Fe", "Chubut", "Mendoza"],
      "y ciudad está más de una vez, mezcla de origen y destino, y sus destinos/origenes se repiten "
        ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5), ("Cordoba", "Santa Fe", 1.2), ("Tucuman", "Chubut", 1.8), ("Cordoba", "Jujuy", 0.5)] "Cordoba") ["Jujuy", "Santa Fe"]
    ]

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

agenciaEj3_copia_sort :: AgenciaDeViajes
agenciaEj3_copia_sort  =
  [ ("Buenos Aires", "Cordoba", 1.5),
    ("Neuquen", "Rio Negro", 1.0),
    ("Mendoza", "Salta", 1.8),
    ("Cordoba", "Mendoza", 1.2),
    ("Salta", "Tucuman", 0.5),
    ("Tucuman", "Santiago del Estero", 0.6),
    ("La Pampa", "Neuquen", 1.4),
    ("Santa Fe", "La Pampa", 1.1),
    ("Santiago del Estero", "Santa Fe", 0.7),
    ("Rio Negro", "Chubut", 1.9)
  ]

agenciaEj3_2 :: AgenciaDeViajes
agenciaEj3_2 =
  [ ("Buenos Aires", "Cordoba", 1.5),
    ("Neuquen", "Rio Negro", 1.0),
    ("Santiago del Estero", "Santa Fe", 0.7),
    ("Cordoba", "Mendoza", 1.2),
    ("Salta", "Tucuman", 0.5),
    ("Rio Negro", "Chubut", 1.900002),
    ("Tucuman", "Santiago del Estero", 0.6),
    ("Santa Fe", "La Pampa", 1.141231),
    ("Mendoza", "Salta", 1.8),
    ("La Pampa", "Neuquen", 1.42222)
  ]

agenciaEj3_2_tiempo_modern :: AgenciaDeViajes
agenciaEj3_2_tiempo_modern =
  [ ("Buenos Aires", "Cordoba", 1.35),
    ("Neuquen", "Rio Negro", 0.9),
    ("Santiago del Estero", "Santa Fe", 0.63),
    ("Cordoba", "Mendoza", 1.08),
    ("Salta", "Tucuman", 0.45),
    ("Rio Negro", "Chubut", 1.7100018),
    ("Tucuman", "Santiago del Estero", 0.54),
    ("Santa Fe", "La Pampa", 1.0271079),
    ("Mendoza", "Salta", 1.62),
    ("La Pampa", "Neuquen", 1.279998)
  ]




-- contemplando permutaciones
testsEjmodernizarFlota :: Test
testsEjmodernizarFlota =
  test
    [ "Flota modernizada -> True"
        ~: funcionTest3 agenciaEj3
        ~?= True,
      expectPermutacion agenciaEj3 agenciaEj3_copia_sort,
      expectlistProximity (funcionTest3ListaFlotantes (modernizarFlota agenciaEj3)) (funcionTest3ListaFlotantes (modernizarFlota agenciaEj3) ),
      expectlistProximity (funcionTest3ListaFlotantes (modernizarFlota agenciaEj3_2)) (funcionTest3ListaFlotantes agenciaEj3_2_tiempo_modern),
      "Flota vacía -> True -> No hay flotas a modernizar"
        ~: funcionTest3 []
        ~?= True
    ]

-- test ejercicio 4
testsEjciudadMasConectada :: Test
testsEjciudadMasConectada =
  test
    [ "ciudadMasConectada, lista de una tupla"
        ~: expectAny (ciudadMasConectada [("BsAs", "Rosario", 5.0)]) ["BsAs", "Rosario"],
      "ciudadMasConectada, lista de dos tuplas con una ciudad con mayor cantidad de vuelos"
        ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0)]
        ~?= "Rosario",
      "ciudadMasConectada, lista de dos tuplas con dos ciudades con la misma cantidad de vuelos"
        ~: expectAny (ciudadMasConectada [("Cordoba", "Tucuman", 5.0), ("Tucuman", "Cordoba", 5.0)]) ["Cordoba", "Tucuman"],
      "ciudadMasConectada, lista de tres tuplas, con una ciudad con mas vuelos que las otras"
        ~: ciudadMasConectada [("BsAs", "Tucuman", 5.0), ("Tucuman", "Jujuy", 6.0), ("Tucuman", "Asuncion", 3.0)]
        ~?= "Tucuman",
      "ciudadMasConectada, lista de tres tuplas, dos ciudades con mas cantidad de vuelos"
        ~: expectAny (ciudadMasConectada [("Madrid", "Barcelona", 2), ("Barcelona", "Roma", 4), ("Paris", "Roma", 3)]) ["Barcelona", "Roma"],
      "ciudadMasConectada, lista de tres tuplas, dos ciudades con mas cantidad de vuelos"
        ~: expectAny (ciudadMasConectada [("Madrid", "Barcelona", 2), ("Barcelona", "Roma", 4), ("Madrid", "Roma", 3)]) ["Madrid", "Barcelona", "Roma"],
      "ciudadMasConectada, lista de cuatro tuplas, una ciudad con mas vuelos que las otras"
        ~: ciudadMasConectada [("Paris", "Barcelona", 4), ("Roma", "BsAs", 3), ("BsAs", "Mexico", 4), ("BsAs", "Barcelona", 2)]
        ~?= "BsAs",
      "ciudadMasConectada, lista de cuatro tuplas, dos ciudades con mas vuelos que las otras"
        ~: expectAny (ciudadMasConectada [("Madrid", "Barcelona", 2), ("Barcelona", "Roma", 4), ("Paris", "Roma", 3), ("Roma", "Barcelona", 4)]) ["Barcelona", "Roma"],
      "ciudadMasConectada, lista de cuatro tuplas, todas las ciudades con el mismo num de vuelos"
        ~: expectAny (ciudadMasConectada [("Tucuman", "Cordoba", 4), ("BsAs", "Rosario", 3), ("Paris", "Barcelona", 2), ("Jujuy", "Salta", 3)]) ["Tucuman", "Cordoba", "BsAs", "Rosario", "Paris", "Barcelona", "Jujuy", "Salta"],
      "ciudadMasConectada, lista de cuatro tuplas, lista de vuelos capicua (?)"
        ~: ciudadMasConectada [("Madrid", "Barcelona", 2), ("Barcelona", "Roma", 4), ("Roma", "Barcelona", 3), ("Barcelona", "Madrid", 4)]
        ~?= "Barcelona"
    ]

-- test ejercicio 5
testsEjsePuedeLlegar :: Test
testsEjsePuedeLlegar =
  test
    [ "No hay vuelos"
        ~: sePuedeLlegar [] "Buenos Aires" "Chubut"
        ~?= False,
      "Se puede llegar caso verdadero con una escala"
        ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba"
        ~?= True,
      "Agencia de viajes tiene 1 elemento"
        ~: (sePuedeLlegar [("Santa Fe", "Buenos Aires", 1.5)] "Chubut" "Santa Fe")
        ~?= False,
      "Agencia de viajes tiene más elementos, no hay niguna ruta y ciudades aparecen"
        ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5), ("Cordoba", "Santa Fe", 1.2), ("Tucuman", "Chubut", 1.8), ("Cordoba", "Jujuy", 0.5)] "Tucuman" "Jujuy")
        ~?= False,
      "No hay niguna ruta ya que las ciudades no aparecen"
        ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5), ("Cordoba", "Santa Fe", 1.2), ("Tucuman", "Chubut", 1.8), ("Cordoba", "Jujuy", 0.5)] "Salta" "La Pampa")
        ~?= False,
      "No hay ninguna ruta, pero el camino aparece al revés"
        ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5), ("Cordoba", "Buenos Aires", 1.2), ("Tucuman", "Chubut", 1.8), ("Cordoba", "Jujuy", 0.5)] "Jujuy" "Cordoba")
        ~?= False,
      "Hay una ruta directa"
        ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5), ("Buenos Aires", "Santa Fe", 1.2), ("Tucuman", "Chubut", 1.8), ("Cordoba", "Jujuy", 0.5)] "Santa Fe" "Cordoba")
        ~?= True,
      "Hay una sola ruta con escala"
        ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5), ("Buenos Aires", "La Pampa", 1.2), ("Tucuman", "Chubut", 1.8), ("Cordoba", "Jujuy", 0.5)] "Santa Fe" "Jujuy")
        ~?= True,
      "Hay forma de llegar con distintas escalas"
        ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5), ("Buenos Aires", "La Pampa", 1.2), ("Tucuman", "Buenos Aires", 1.8), ("Cordoba", "Jujuy", 0.5), ("Cordoba", "Buenos Aires", 2.5), ("Santa Fe", "Tucuman", 4.5)] "Santa Fe" "Buenos Aires")
        ~?= True,
      "Hay escala y ruta directa al mismo tiempo"
        ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5), ("Buenos Aires", "La Pampa", 1.2), ("Tucuman", "Buenos Aires", 1.8), ("Cordoba", "Jujuy", 0.5), ("Cordoba", "Buenos Aires", 2.5), ("Santa Fe", "Jujuy", 4.5), ("Santa Fe", "Buenos Aires", 2.5)] "Santa Fe" "Buenos Aires")
        ~?= True
    ]

-- test ejercicio 6
agenciaEj4 :: AgenciaDeViajes
agenciaEj4 = agenciaEj3

testsEjduracionDelCaminoMasRapido :: Test
testsEjduracionDelCaminoMasRapido =
  test
    [ "Vuelo directo -> True -> duración del vuelo"
        ~: funcionTest6 True agenciaEj4 "La Pampa" "Neuquen"
        ~?= True,
      "Vuelo con escala -> True -> duración de ambos vuelos"
        ~: funcionTest6 False agenciaEj4 "Cordoba" "Salta"
        ~?= True
    ]

-- test ejercicio 7
{-
  A las agencias se les ah puesto valor de tiempo 1 porque para el // los test, carecen de interes
-}
agencia :: AgenciaDeViajes
agencia =
  [ ("Buenos Aires", "Cordoba", 1),
    ("Cordoba", "Santa Fe", 1),
    ("Santa Fe", "Neuquen", 1),
    ("Neuquen", "Misiones", 1),
    ("Misiones", "Jujuy", 1),
    ("Jujuy", "Corrientes", 1),
    ("Corrientes", "Santiago Del Estero", 1),
    ("Santiago Del Estero", "Chaco", 1),
    ("Chaco", "Chubut", 1),
    ("Chubut", "La Pampa", 1),
    ("La Pampa", "Mendoza", 1),
    ("Mendoza", "San Juan", 1),
    ("San Juan", "Santa Cruz", 1),
    ("Santa Cruz", "Tucuman", 1),
    ("Tucuman", "Rio Negro", 1),
    ("Rio Negro", "Formosa", 1),
    ("Formosa", "Entre Rios", 1),
    ("Entre Rios", "Catamarca", 1),
    ("Catamarca", "La Rioja", 1),
    ("La Rioja", "Buenos Aires", 1)
  ]

agencia2 :: AgenciaDeViajes
agencia2 =
  [ ("Cordoba", "Santa Fe", 1),
    ("Santa Fe", "Cordoba", 1),
    ("Misiones", "Cordoba", 1),
    ("Buenos Aires", "Cordoba", 1),
    ("Buenos Aires", "Neuquen", 1),
    ("Neuquen", "Santa Fe", 1),
    ("Neuquen", "Santa Fe", 1),
    ("Chubut", "Buenos Aires", 1),
    ("Neuquen", "Buenos Aires", 1),
    ("Buenos Aires", "Santa Fe", 1),
    ("Misiones", "Buenos Aires", 1)
  ]

agencia3 :: AgenciaDeViajes
agencia3 =
  [ ("Buenos Aires", "Cordoba", 1),
    ("Cordoba", "Mendoza", 1),
    ("La Pampa", "Cordoba", 1),
    ("La Pampa", "Buenos Aires", 1),
    ("Buenos Aires", "La Pampa", 1),
    ("Mendoza", "Cordoba", 1),
    ("Chubut", "Cordoba", 1),
    ("Rio Negro", "Buenos Aires", 1),
    ("Buenos Aires", "Mendoza", 1),
    ("La Pampa", "Mendoza", 1),
    ("Cordoba", "Chubut", 1),
    ("Chubut", "Buenos Aires", 1)
  ]

agenciaSinConexion :: AgenciaDeViajes
agenciaSinConexion =
  [ ("Buenos Aires", "Cordoba", 1),
    ("Cordoba", "Santa Fe", 1),
    ("Santa Fe", "Neuquen", 1),
    ("Neuquen", "Misiones", 1),
    ("Jujuy", "Corrientes", 1),
    ("Corrientes", "Santiago Del Estero", 1),
    ("Santiago Del Estero", "Chaco", 1),
    ("Chaco", "Chubut", 1),
    ("Chubut", "La Pampa", 1),
    ("La Pampa", "Mendoza", 1),
    ("Mendoza", "San Juan", 1),
    ("San Juan", "Santa Cruz", 1),
    ("Santa Cruz", "Tucuman", 1),
    ("Tucuman", "Rio Negro", 1),
    ("Rio Negro", "Formosa", 1),
    ("Formosa", "Entre Rios", 1),
    ("Entre Rios", "Catamarca", 1),
    ("Catamarca", "La Rioja", 1),
    ("La Rioja", "Buenos Aires", 1)
  ]

testsEjpuedoVolverAOrigen :: Test
testsEjpuedoVolverAOrigen =
  test
    [ "valor verdadero"
        ~: puedoVolverAOrigen agencia "Buenos Aires"
        ~?= True,
      "valor verdadero"
        ~: puedoVolverAOrigen agencia2 "Buenos Aires"
        ~?= True,
      "valor verdadero"
        ~: puedoVolverAOrigen agencia3 "Buenos Aires"
        ~?= True,
      "valor falso"
        ~: puedoVolverAOrigen agenciaSinConexion "Buenos Aires"
        ~?= False,
      "valor falso -> no hay vuelos"
        ~: puedoVolverAOrigen [] "Buenos Aires"
        ~?= False,
      "test aux"
        ~: testFiltradoFiltradosAux
    ]

-- test ejercicio 7 aux
testFiltradoFiltradosAux :: Test
testFiltradoFiltradosAux =
  test
    [ -- conMismoOrigen
      "con mismo origen -> origen = Buenos Aires -> lista True"
        ~: length (conMismoOrigen agencia "Buenos Aires")
        ~?= 1,
      "con mismo origen -> origen = Cordoba -> lista True"
        ~: length (conMismoOrigen agencia3 "Cordoba")
        ~?= 2,
      "con mismo origen -> lista vacia Cordoba -> []"
        ~: length (conMismoOrigen [] "Cordoba")
        ~?= 0,
      -- conMismoDestino
      "con mismo destino -> destino = Buenos Aires -> lista True"
        ~: length (conMismoDestino agencia2 "Buenos Aires")
        ~?= 3,
      "con mismo destino -> destino = Cordoba -> lista True"
        ~: length (conMismoDestino agencia3 "Cordoba")
        ~?= 4,
      "con mismo destino -> lista vacia Cordoba -> []"
        ~: length (conMismoDestino [] "Cordoba")
        ~?= 0,
      -- diferenteACiudad
      "con diferente origen y destino -> ciudad = Buenos Aires -> lista True"
        ~: length (diferenteACiudad agencia2 "Buenos Aires")
        ~?= 5,
      "con diferente origen y destino -> ciudad = Cordoba -> lista True"
        ~: length (diferenteACiudad agencia3 "Cordoba")
        ~?= 6,
      "con diferente origen y destino -> lista vacia Cordoba -> []"
        ~: length (diferenteACiudad [] "Cordoba")
        ~?= 0,
      -- diferenteA2Ciudades
      "con diferente origen y destino -> ciudad = Buenos Aires -> lista True"
        ~: length (diferenteA2Ciudades agencia2 "Buenos Aires" "Chubut")
        ~?= 8,
      "con diferente origen y destino -> ciudad = Cordoba -> lista True"
        ~: length (diferenteA2Ciudades agencia3 "Cordoba" "Mendoza")
        ~?= 8,
      "con diferente origen y destino -> lista vacia Cordoba -> []"
        ~: length (diferenteA2Ciudades [] "Cordoba" "Mendoza")
        ~?= 0
    ]

-------
-- Funciones extras
reverso :: [a] -> [a]
reverso [] = []
reverso (x : xs) = reverso xs ++ [x]

-- funciones ejercicio 3
-- ahorro de espacio con funciones con nombre mas chico
funcionTest3 :: AgenciaDeViajes -> Bool
funcionTest3 x = validaPorVueloModernizado (modernizarFlota x) (reverso x)

funcionTest3ListaFlotantes :: AgenciaDeViajes -> [Float]
funcionTest3ListaFlotantes [] = []
funcionTest3ListaFlotantes ((_, _, t):xs) = t: funcionTest3ListaFlotantes xs 

{-
  En esta funcion validamos de que esten los vuelos como tal y sus valores sean los mismos que la llamada de la función sin importar el orden
  quitando de ambas listas (de existir) los vuelos hasta llegar a un valor en comun de los casos base
-}
validaPorVueloModernizado :: AgenciaDeViajes -> AgenciaDeViajes -> Bool
validaPorVueloModernizado [] [] = True
validaPorVueloModernizado [] _ = False
validaPorVueloModernizado _ [] = False
validaPorVueloModernizado vuelosModernizados (vuelo : vuelosOriginales) =
  validaPorVueloModernizado (quitaVueloDeAgencia vuelosModernizados vuelo) vuelosOriginales

quitaVueloDeAgencia :: AgenciaDeViajes -> Vuelo -> AgenciaDeViajes
quitaVueloDeAgencia [] _ = []
quitaVueloDeAgencia (vuelo : vuelos) (o, d, t)
  | not esVueloModernizado = vuelo : quitaVueloDeAgencia vuelos (o, d, t)
  | otherwise = vuelos
  where
    esVueloModernizado = vuelo == (o, d, t * 0.9)

-- funciones ejercicio 6

{-
  "Simple" -> toma un Bool para ver si es por vuelo directo o con escala
    true = directo
    false = escala

  en caso de ser con escala (simple == false)
    - corroboramos si la duracion del "caminoMasRapido" es igual a la minima duracion de la lista
    de duraciones que nos devuelve "vueloConEscala"
-}
funcionTest6 :: Bool -> AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
funcionTest6 simple viajes origen destino
  | simple = duracionDelCaminoMasRapido viajes origen destino == vueloDirecto viajes origen destino
  | otherwise = duracionDelCaminoMasRapido viajes origen destino == minimaDuracion listaDeTiemposEnEscala
  where
    conOrigenIgual = conMismoOrigen viajes origen
    conDestinoIgual = conMismoDestino viajes destino
    listaDeTiemposEnEscala = vueloConEscala conOrigenIgual conDestinoIgual

{-
  si pide por vuelo directo, toma solo el vuelo que coincide y devuelve su duración

  al ser un vuelo directo y tener como require "vuelosValidos" la misma no puede haber otro vuelo que coincida con éste
  por tanto, devuelve el primer vuelo coincidente
-}
vueloDirecto :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
vueloDirecto ((origen, destino, t) : vuelos) cdOrigen cdDestino
  | origen == cdOrigen && destino == cdDestino = t
  | otherwise = vueloDirecto vuelos cdOrigen cdDestino

{-
  usando previamente un filtrado por origen y destino igual a
  la ciudad de la que se parte y la ciudad a la que se quiere ir (respectivamente)
  
  la primer agencia de viajes toma las de origen igual a la ciudadOrigen
  la segunda agencia de viajes toma las de destino igual a la ciudadDestino

  validamos si el destino de la primera es igual al origen de la segunda
    (o,_,_) (_,d,_) / o == d
  
  toma la suma de los tiempos y lo agrega a una lista

-}
vueloConEscala :: AgenciaDeViajes -> AgenciaDeViajes -> [Duracion]
vueloConEscala [] _ = []
vueloConEscala _ [] = []
vueloConEscala ((origen1, destino1, t1) : viajes1) ((origen2, destino2, t2) : viajes2)
  | destinoYOrigen = t1 + t2 : vueloConEscala viajes1 viajes2
  | otherwise = vueloConEscala viajes1 ((origen2, destino2, t2) : viajes2) ++ vueloConEscala ((origen1, destino1, t1) : viajes1) viajes2
  where
    destinoYOrigen = destino1 == origen2

-----
-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat :: Float
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity :: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x : xs) (y : ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat

-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac, af) ((bc, bf) : bs) = sonAprox || (elemAproxTupla (ac, af) bs)
  where
    sonAprox = (ac == bc) && (aproximado af bf)

-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: (Ord a) => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)