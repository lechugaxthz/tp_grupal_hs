import Prueba
import Solucion
import Test.HUnit

agencia = [("a", "b", 1), ("b", "c", 1), ("c", "d", 1), ("d", "e", 1), ("e", "f", 1), ("f", "g", 1), ("g", "h", 1), ("h", "i", 1), ("i", "j", 1), ("j", "k", 1), ("k", "l", 1), ("l", "m", 1), ("m", "n", 1), ("n", "o", 1), ("o", "p", 1), ("p", "q", 1), ("q", "r", 1), ("r", "s", 1), ("s", "t", 1), ("t", "a", 1)]
agencia2 = [("b","c",1),("c","b",1),("e","b",1),("a","b",1),("a","d",1),("d","c",1),("d","c",1),("j","a",1),("d","a",1),("a","c",1),("e","a",1)]
agenciaSinConexion = [("a", "b", 1), ("b", "c", 1), ("c", "d", 1), ("d", "e", 1), ("f", "g", 1), ("g", "h", 1), ("h", "i", 1), ("i", "j", 1), ("j", "k", 1), ("k", "l", 1), ("l", "m", 1), ("m", "n", 1), ("n", "o", 1), ("o", "p", 1), ("p", "q", 1), ("q", "r", 1), ("r", "s", 1), ("s", "t", 1), ("t", "a", 1)]

testVueloIdaYVuelta :: Test
testVueloIdaYVuelta =
  test
    [
      "valor verdadero" ~: puedoVolverAOrigen agencia "a" ~?= True,
      "valor verdadero" ~: puedoVolverAOrigen agencia2 "a" ~?= True,
      "valor falso" ~: puedoVolverAOrigen agenciaSinConexion "a" ~?= False
    ]

runTest = runTestTT testVueloIdaYVuelta