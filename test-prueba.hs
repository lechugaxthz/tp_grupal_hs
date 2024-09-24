import Prueba
import Test.HUnit

agencia = [("a", "b", 1), ("b", "c", 1), ("c", "d", 1), ("d", "e", 1), ("e", "f", 1), ("f", "g", 1), ("g", "h", 1), ("h", "i", 1), ("i", "j", 1), ("j", "k", 1), ("k", "l", 1), ("l", "m", 1), ("m", "n", 1), ("n", "o", 1), ("o", "p", 1), ("p", "q", 1), ("q", "r", 1), ("r", "s", 1), ("s", "t", 1), ("t", "a", 1)]

agenciaSinConexion = [("a", "b", 1), ("b", "c", 1), ("c", "d", 1), ("d", "e", 1), ("f", "g", 1), ("g", "h", 1), ("h", "i", 1), ("i", "j", 1), ("j", "k", 1), ("k", "l", 1), ("l", "m", 1), ("m", "n", 1), ("n", "o", 1), ("o", "p", 1), ("p", "q", 1), ("q", "r", 1), ("r", "s", 1), ("s", "t", 1), ("t", "a", 1)]

testVueloIdaYVuelta :: Test
testVueloIdaYVuelta =
  test
    [ "valor verdadero" ~: pruebaIdaYVuelta agencia "a" ~?= True,
      "valor falso" ~: pruebaIdaYVuelta agenciaSinConexion "a" ~?= False
    ]

runTest = runTestTT testVueloIdaYVuelta