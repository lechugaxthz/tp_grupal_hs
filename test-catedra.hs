import Data.List
import Solucion
import Test.HUnit

-- No está permitido agregar nuevos imports.

runCatedraTests :: IO Counts
runCatedraTests = runTestTT allTests


allTests :: Test
allTests =
  test
    [ "vuelosValidos" ~: testsEjvuelosValidos,
      "ciudadesConectadas" ~: testsEjciudadesConectadas,
      "modernizarFlota" ~: testsEjmodernizarFlota,
      "ciudadMasConectada" ~: testsEjciudadMasConectada,
      "sePuedeLlegar" ~: testsEjsePuedeLlegar,
      "duracionDelCaminoMasRapido" ~: testsEjduracionDelCaminoMasRapido,
      "puedoVolverAOrigen" ~: testsEjpuedoVolverAOrigen
    ]

-- corregir los tests si es necesario con las funciones extras que se encuentran al final del archivo

testsEjvuelosValidos :: Test
testsEjvuelosValidos =
  test
    [ "vuelos válido con un elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True
    ]

-- test ejercicio 2
testsEjciudadesConectadas = test [
    "ciudad conectada con un elemento" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0)] "Rosario" ~?= ["BsAs"],
    "Agencia de viajes tiene 1 elemento, ciudad no está" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires","Cordoba",1.5)] "Rosario")  [],
    "Agencia de viajes tiene 1 elemento, ciudad es origen" ~: expectPermutacion(ciudadesConectadas [("Buenos Aires","Cordoba",1.5)] "Buenos Aires")  ["Cordoba"],
    "Agencia de viajes tiene 1 elemento, ciudad es destino" ~: expectPermutacion(ciudadesConectadas [("Buenos Aires","Cordoba",1.5)] "Cordoba")  ["Buenos Aires"],
    "Agencia de viajes varios elementos, ciudad no está" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Cordoba", "Mendoza", 1.2),("Mendoza", "Salta", 1.8),("Salta", "Tucuman", 0.5)] "Chubut")  [],
    "y ciudad está 1 vez como origen, su destino no se repite en otro vuelo" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Chubut", "Mendoza", 1.2),("Mendoza", "Salta", 1.8),("Salta", "Tucuman", 0.5)] "Buenos Aires" )  ["Cordoba"],
    "y ciudad está 1 vez como origen, su destino se repite en otro vuelo" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Cordoba", "Mendoza", 1.2),("Mendoza", "Salta", 1.8),("Salta", "Tucuman", 0.5)] "Buenos Aires" )  ["Cordoba"],
    "y ciudad está 1 vez como destino, su origen no se repite en otro vuelo" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Chubut", "Mendoza", 1.2),("Tucuman", "Salta", 1.8),("Salta", "Tucuman", 0.5)] "Mendoza" )  ["Chubut"],
    "y ciudad está 1 vez como destino, su origen se repite en otro vuelo" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Chubut", "Mendoza", 1.2),("Tucuman", "Chubut", 1.8),("Salta", "Tucuman", 0.5)] "Mendoza" )  ["Chubut"],
    "y ciudad está más de una vez, solo como origen, y destinos no se repiten" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Buenos Aires", "Mendoza", 1.2),("Tucuman", "Chubut", 1.8),("Salta", "Tucuman", 0.5)] "Buenos Aires" )  ["Cordoba","Mendoza"],
    "y ciudad está más de una vez, solo como origen, y sus destinos se repiten" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Buenos Aires", "Mendoza", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Mendoza", 0.5)] "Buenos Aires" )  ["Cordoba","Mendoza"],
    "y ciudad está más de una vez, solo como destino, y sus origenes no se repiten" ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "Mendoza", 1.2),("Tucuman", "Cordoba", 1.8),("Salta", "Jujuy", 0.5)] "Cordoba" )  ["Santa Fe","Tucuman"],
    "y ciudad está más de una vez, solo como destino, y sus origenes se repiten" ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "Santa Fe", 1.2),("Tucuman", "Cordoba", 1.8),("Tucuman", "Jujuy", 0.5)] "Cordoba")  ["Santa Fe","Tucuman"],
    "y ciudad está más de una vez, mezcla de origen y destino, y sus destinos/origenes no se repiten" ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5),("Cordoba", "Mendoza", 1.2),("Cordoba", "Chubut", 1.8),("Salta", "Jujuy", 0.5) ] "Cordoba")  ["Santa Fe","Chubut","Mendoza"],
    "y ciudad está más de una vez, mezcla de origen y destino, y sus destinos/origenes se repiten " ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5),("Cordoba", "Santa Fe", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Cordoba")  ["Jujuy","Santa Fe"]
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

testsEjmodernizarFlota :: Test
testsEjmodernizarFlota =
  test
    [ "Flota modernizada -> True"
        ~: funcionTest3 agenciaEj3
        ~?= True,
      "Flota vacía -> True -> No hay flotas a modernizar"
        ~: funcionTest3 []
        ~?= True
    ]

testsEjciudadMasConectada :: Test
testsEjciudadMasConectada =
  test
    [ "ciudad Mas conectada que aparece dos veces" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0)] ~?= "Rosario"
    ]
-- test ejercicio 5
testsEjsePuedeLlegar = test [
    "Se puede llegar caso verdadero con una escala" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= True,
    "Agencia de viajes tiene 1 elemento" ~: (sePuedeLlegar [("Santa Fe","Buenos Aires",1.5)] "Chubut" "Santa Fe") ~?= False,
    "Agencia de viajes tiene más elementos, no hay niguna ruta y ciudades aparecen" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Cordoba", "Santa Fe", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Tucuman" "Jujuy") ~?= False,
    "No hay niguna ruta ya que las ciudades no aparecen" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Cordoba", "Santa Fe", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Salta" "La Pampa") ~?= False,
    "No hay ninguna ruta, pero el camino aparece al revés" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Cordoba", "Buenos Aires", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Jujuy" "Cordoba") ~?= False,
    "Hay una ruta directa" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "Santa Fe", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Santa Fe" "Cordoba") ~?= True,
    "Hay una sola ruta con escala" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "La Pampa", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Santa Fe" "Jujuy") ~?= True,
    "Hay forma de llegar con distintas escalas" ~: (sePuedeLlegar  [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "La Pampa", 1.2),("Tucuman", "Buenos Aires", 1.8),("Cordoba", "Jujuy", 0.5),("Cordoba", "Buenos Aires", 2.5),("Santa Fe", "Tucuman", 4.5)] "Santa Fe" "Buenos Aires") ~?= True,
    "Hay escala y ruta directa al mismo tiempo" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "La Pampa", 1.2),("Tucuman", "Buenos Aires", 1.8),("Cordoba", "Jujuy", 0.5),("Cordoba", "Buenos Aires", 2.5),("Santa Fe", "Jujuy", 4.5),("Santa Fe", "Buenos Aires", 2.5)] "Santa Fe" "Buenos Aires") ~?= True


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
  toma un Bool para ver si es por vuelo directo o con escala
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
-}
vueloDirecto :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
vueloDirecto ((origen, destino, t) : vuelos) cdOrigen cdDestino
  | origen == cdOrigen && destino == cdDestino = t
  | otherwise = vueloDirecto vuelos cdOrigen cdDestino

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