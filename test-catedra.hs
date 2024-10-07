import Test.HUnit
import Data.List
import Solucion
-- No está permitido agregar nuevos imports.


runCatedraTests = runTestTT allTests

correr = runTestTT testsEjciudadesConectadas
correr2 = runTestTT testsEjsePuedeLlegar

allTests = test [
    "vuelosValidos" ~: testsEjvuelosValidos,
    "ciudadesConectadas" ~: testsEjciudadesConectadas,
    "modernizarFlota" ~: testsEjmodernizarFlota,
    "ciudadMasConectada" ~: testsEjciudadMasConectada,
    "sePuedeLlegar" ~: testsEjsePuedeLlegar,
    "duracionDelCaminoMasRapido" ~: testsEjduracionDelCaminoMasRapido,
    "puedoVolverAOrigen" ~: testsEjpuedoVolverAOrigen
    ]

-- corregir los tests si es necesario con las funciones extras que se encuentran al final del archivo

testsEjvuelosValidos = test [
    "vuelos válido con un elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True
    ]
-- Ejercicio 2
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


testsEjmodernizarFlota = test [
    "flota modernizada con un elemento" ~: modernizarFlota [("BsAs", "Rosario", 10.0)] ~?= [("BsAs", "Rosario", 9.0)]
    ]

testsEjciudadMasConectada = test [
    "ciudad Mas conectada que aparece dos veces" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0)] ~?= "Rosario"
    ]

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
    ]

testsEjduracionDelCaminoMasRapido = test [
    "duración del camino más rápido con una escala" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= 10.0
    ]

testsEjpuedoVolverAOrigen = test [
        "puedo volver a origen caso verdadero con una escala" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" ~?= True
    ]


-- Funciones extras

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
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

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
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)