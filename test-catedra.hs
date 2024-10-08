import Test.HUnit
import Data.List
import Solucion
import Solucion (ciudadMasConectada)
-- No está permitido agregar nuevos imports.


runCatedraTests = runTestTT allTests

allTests = test [
    "vuelosValidos" ~: testsEjvuelosValidos,
    "vueloValido" ~: testsEjvueloValido,
    "ciudadesConectadas" ~: testsEjciudadesConectadas,
    "modernizarFlota" ~: testsEjmodernizarFlota,
    "ciudadMasConectada" ~: testsEjciudadMasConectada,
    "sePuedeLlegar" ~: testsEjsePuedeLlegar,
    "duracionDelCaminoMasRapido" ~: testsEjduracionDelCaminoMasRapido,
    "puedoVolverAOrigen" ~: testsEjpuedoVolverAOrigen
    ]

-- corregir los tests si es necesario con las funciones extras que se encuentran al final del archivo

testsEjvuelosValidos = test [
    "vuelos válidos con 1 elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True,
    "vuelos no válidos con 1 elemento, tiempo de vuelo 0" ~: vuelosValidos [("BsAs","Rosario",0)] ~?= False,
    "vuelos no válidos con 1 elemento, ciudades repetidas" ~: vuelosValidos [("Rosario","Rosario",5)] ~?= False,
    "vuelos válidos con 2 elementos distintos" ~: vuelosValidos [("Rosario","Cordoba",4),("Tucuman","Cordoba",3)] ~?= True,
    "vuelos válidoss con 2 elementos, mismo viaje ida y vuelta" ~: vuelosValidos [("BsAs","Cordoba",4),("Cordoba","Rosario",4)] ~?= True,
    "vuelos no válidos con 2 elementos, mismo viaje repetido" ~: vuelosValidos [("BsAs","Tucuman",5),("BsAs","Tucuman",5)] ~?= False,
    "vuelos no válidos con 2 elementos, mismo viaje con distinto tiempo" ~: vuelosValidos [("Tucuman","Salta",2),("Tucuman","Salta",3)] ~?= False,
    "vuelos válidos con 3 elementos,elementos distintos" ~: vuelosValidos [("BsAs","Tucuman",5),("Jujuy","BsAs",4),("Salta","Resistencia",3)] ~?= True,
    "vuelos válidos con 3 elementos, dos ida y vuelta y un distinto" ~: vuelosValidos [("Tucuman","BsAs",4),("BsAs","Tucuman",4),("Montevideo","Rosario",3)] ~?= True,
    "vuelos válidos con 3 elementos, dos ida y vuelta y un distinto CASO DOS" ~: vuelosValidos [("Rosario","Damasco",4),("Montevideo","Rosario",3),("Damasco","Rosario",4)] ~?= True,
    "vuelos no válidos con 3 elementos, todos iguales" ~: vuelosValidos [("Rosario","Tucuman",4),("Rosario","Tucuman",4),("Rosario","Tucuman",4)] ~?= False,
    "vuelos no válidos con 3 elementos, dos iguales y uno distinto"~: vuelosValidos [("Rosario","Tucuman",1),("Tucuman","Cordoba",2),("Rosario","Tucuman",1)] ~?= False,
    "vuelos no válidos con 3 elementos, dos iguales con distinto tiempo" ~: vuelosValidos [("Montevideo","BsAs",4),("Montevideo","BsAs",5),("Rosario","Santa Fe",1)] ~?= False,
    "vuelos no válidos con 3 elementos, uno con tiempo de vuelo 0" ~:vuelosValidos [("Montevideo","BsAs",1),("Tucuman","Posadas",2),("Tucuman","Salta",0)] ~?= False,
    "vuelos no válidos con 3 elementos, todos con tiempo de vuelo 0" ~:vuelosValidos [("Nueva York","Rosario",0),("Tokyo","Formosa",0),("Pekin","Ushuaia",0)] ~?= False,
    "vuelos no válidos con 3 elementos, dos con tiempo de vuelo 0" ~: vuelosValidos [("Rosario","Damasco",1),("Tucuman","Cordoba",0),("Damasco","Tucuman",0)] ~?= False,
    "vuelos no válidos con 3 elementos, dos con tiempo de vuelo 0 CASO DOS" ~: vuelosValidos [("Rosario","Damascco",0),("Tokyo","Formosa",4),("Berlin","Rawson",0)] ~?= False,
    "vuelos válidos con 4 elementos, todos distintos" ~: vuelosValidos [("Tucuman","La Plata",3),("Rosario","Tokyo",4),("Tokyo","Nueva York",2),("Helsinki","Barcelona",2)] ~?= True,
    "vuelos válidos con 4 elementos, dos idas y dos vueltas" ~: vuelosValidos [("Tucuman","Helsinki",2),("Rosario","Rawson",3),("Helsinki","Tucuman",2),("Rawson","Rosario",3)] ~?= True,
    "vuelos válidos con 4 elementos, dos idas y dos vueltas CASO DOS" ~: vuelosValidos [("Helsinki","Teheran",2),("Rawson","Tucuman",4),("Tucuman","Rawson",4),("Teheran","Helsinki",2)] ~?= True,
    "vuelos válidos con 4 elementos, dos idas y dos vueltas CASO TRES" ~: vuelosValidos [("Helsinki","Tucuman",2),("Tucuman","Helsinki",2),("Rosario","Catamarca",3),("Catamarca","Rosario",3)] ~?= True,
    "vuelos no válidos con 4 elementos, todos iguales" ~: vuelosValidos [("Ciudad1","Ciudad2",4),("Ciudad1","Ciudad2",4),("Ciudad1","Ciudad2",4),("Ciudad1","Ciudad2",4)] ~?= False,
    "vuelos no válidos con 4 elementos, dos iguales dos distintos" ~: vuelosValidos [("Ciudad1","Ciudad2",3),("Ciudad3","Ciudad4",5),("Ciudad1","Ciudad2",3),("Ciudad3","Ciudad4",5)] ~?= False,
    "vuelos no válidos con 4 elementos, dos iguales dos distintos con distinto tiempo" ~: vuelosValidos [("Ciudad1","Ciudad2",3),("Ciudad3","Ciudad4",5),("Ciudad1","Ciudad2",4),("Ciudad3","Ciudad4",6)] ~?= False
    ]
testsEjvueloValido :: Test
testsEjvueloValido = test [
    "Tupla con dos ciudades distintas y t>0" ~: vueloValido ("Rosario","Cordoba",3) ~?= True,
    "Tupla con dos ciudades iguales y t>0" ~: vueloValido ("Rosario","Rosario",2) ~?= False,
    "Tupla con dos ciudades distintas y t < 0" ~: vueloValido ("Tucuman","Rosario",0) ~?= False,
    "Tupla con dos ciudades distintas y t < 0 CASO DOS" ~: vueloValido ("Rosario","Cordoba",-2) ~?= False,
    "Tupla con dos ciudades iguales y t < 0" ~: vueloValido ("Rosario","Rosario",0) ~?= False,
    "Tupla con dos ciudades iguales y t < 0 CASO DOS" ~: vueloValido ("Rosario","Rosario",-4) ~?= False 
    ]

testsEjciudadesConectadas = test [
    "ciudad conectada con un elemento" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0)] "Rosario" ~?= ["BsAs"]
    ]

testsEjmodernizarFlota = test [
    "flota modernizada con un elemento" ~: modernizarFlota [("BsAs", "Rosario", 10.0)] ~?= [("BsAs", "Rosario", 9.0)]
    ]

testsEjciudadMasConectada = test [
    "ciudadMasConectada, lista de una tupla" ~: expectAny (ciudadMasConectada [("BsAs","Rosario",5.0)]) ["BsAs","Rosario"],
    "ciudadMasConectada, lista de dos tuplas con una ciudad con mayor cantidad de vuelos" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0)] ~?= "Rosario",
    "ciudadMasConectada, lista de dos tuplas con dos ciudades con la misma cantidad de vuelos"  ~: expectAny (ciudadMasConectada [("Cordoba","Tucuman",5.0),("Tucuman","Cordoba",5.0)]) ["Cordoba","Tucuman"],
    "ciudadMasConectada, lista de tres tuplas, con una ciudad con mas vuelos que las otras"  ~: ciudadMasConectada [("BsAs","Tucuman",5.0),("Tucuman","Jujuy",6.0),("Tucuman","Asuncion",3.0)] ~?= "Tucuman",
    "ciudadMasConectada, lista de tres tuplas, dos ciudades con mas cantidad de vuelos" ~: expectAny (ciudadMasConectada [("Madrid","Barcelona",2),("Barcelona","Roma",4),("Paris","Roma",3)]) ["Barcelona","Roma"],
    "ciudadMasConectada, lista de tres tuplas, dos ciudades con mas cantidad de vuelos" ~: expectAny (ciudadMasConectada [("Madrid","Barcelona",2),("Barcelona","Roma",4),("Madrid","Roma",3)]) ["Madrid","Barcelona","Roma"],
    "ciudadMasConectada, lista de cuatro tuplas, una ciudad con mas vuelos que las otras" ~: ciudadMasConectada [("Paris","Barcelona",4),("Roma","BsAs",3),("BsAs","Mexico",4),("BsAs","Barcelona",2)] ~?= "BsAs",
    "ciudadMasConectada, lista de cuatro tuplas, dos ciudades con mas vuelos que las otras" ~: expectAny (ciudadMasConectada [("Madrid","Barcelona",2),("Barcelona","Roma",4),("Paris","Roma",3),("Roma","Barcelona",4)]) ["Barcelona","Roma"],
    "ciudadMasConectada, lista de cuatro tuplas, todas las ciudades con el mismo num de vuelos"  ~: expectAny (ciudadMasConectada[("Tucuman","Cordoba",4),("BsAs","Rosario",3),("Paris","Barcelona",2),("Jujuy","Salta",3)]) ["Tucuman","Cordoba","BsAs","Rosario","Paris","Barcelona","Jujuy","Salta"],
    "ciudadMasConectada, lista de cuatro tuplas, lista de vuelos capicua (?)"  ~: ciudadMasConectada[("Madrid","Barcelona",2),("Barcelona","Roma",4),("Roma","Barcelona",3),("Barcelona","Madrid",4)] ~?= "Barcelona"
    ]

testsEjsePuedeLlegar = test [
    "Se puede llegar caso verdadero con una escala" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= True
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