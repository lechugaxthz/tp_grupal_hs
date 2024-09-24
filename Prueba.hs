module Prueba where

import Solucion (AgenciaDeViajes, Ciudad)

pruebaIdaYVuelta :: AgenciaDeViajes -> Ciudad -> Bool
pruebaIdaYVuelta viajes ciudad = validaMasEscalas2 [conMismoOrigen viajes ciudad, diferenteACiudad viajes ciudad, conMismoDestino viajes ciudad]

conMismoOrigen :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
conMismoOrigen [] _ = []
conMismoOrigen ((origen, destino, t) : viajes) ciudad
  | mismoOrigenYCiudad = (origen, destino, t) : conMismoOrigen viajes ciudad
  | otherwise = conMismoOrigen viajes ciudad
  where
    mismoOrigenYCiudad = origen == ciudad

conMismoDestino :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
conMismoDestino [] _ = []
conMismoDestino ((origen, destino, t) : viajes) ciudad
  | mismoDestinoYCiudad = (origen, destino, t) : conMismoDestino viajes ciudad
  | otherwise = conMismoDestino viajes ciudad
  where
    mismoDestinoYCiudad = destino == ciudad

diferenteACiudad :: AgenciaDeViajes -> Ciudad -> AgenciaDeViajes
diferenteACiudad [] _ = []
diferenteACiudad ((origen, destino, t) : viajes) ciudad
  | ciudadDiferente = (origen, destino, t) : diferenteACiudad viajes ciudad
  | otherwise = diferenteACiudad viajes ciudad
  where
    ciudadDiferente = origen /= ciudad && destino /= ciudad

diferenteA2Ciudades :: AgenciaDeViajes -> Ciudad -> Ciudad -> AgenciaDeViajes
diferenteA2Ciudades [] _  _ = []
diferenteA2Ciudades ((origen, destino, t) : viajes) ciudad1 ciudad2
  | difDestinoYC1 && difOrigenYC2 = (origen, destino, t) : diferenteA2Ciudades viajes ciudad1 ciudad2
  | otherwise = diferenteA2Ciudades viajes ciudad1 ciudad2
  where
    difDestinoYC1 = destino /= ciudad1
    difOrigenYC2 = origen /= ciudad2

sacaOrigenYDestino :: [AgenciaDeViajes] -> Ciudad -> Bool
sacaOrigenYDestino ([] : _ : _ : _) ciudad = False
sacaOrigenYDestino (_ : _ : [] : _) ciudad = False
sacaOrigenYDestino (((origen1, destino1, t1) : origenIgual) : diferenteTodo : ((origen3, destino3, t3) : destinoIgual) : conjunto) ciudad
  | unaEscala = True
  | masEscalas && esOrigen1Ydestino3 = validaMasEscalas (destino1, diferenteTodo, origen3) || sacaOrigenYDestino (((origen1, destino1, t1) : origenIgual) : diferenteTodo : destinoIgual : conjunto) ciudad || sacaOrigenYDestino (origenIgual : diferenteTodo : ((origen3, destino3, t3) : destinoIgual) : conjunto) ciudad
  | otherwise = validaMasEscalas (destino3, diferenteTodo, origen1) || sacaOrigenYDestino (((origen1, destino1, t1) : origenIgual) : diferenteTodo : destinoIgual : conjunto) ciudad || sacaOrigenYDestino (origenIgual : diferenteTodo : ((origen3, destino3, t3) : destinoIgual) : conjunto) ciudad
  where
    unaEscala = (origen1 == ciudad && destino3 == ciudad && destino1 == origen3)
    masEscalas = origen1 /= origen3 && (origen1 == ciudad || origen3 == ciudad) && destino1 /= destino3 && (destino1 == ciudad || destino3 == ciudad)
    esOrigen1Ydestino3 = origen1 == destino3 && origen1 == ciudad

validaMasEscalas :: (Ciudad, AgenciaDeViajes, Ciudad) -> Bool
validaMasEscalas (_, [], _) = False
validaMasEscalas (destinoA, (origen, destino, _) : viajes, origenB)
  | igualAlDestino && igualAlOrigen = True
  | igualAlDestino = validaMasEscalas (destino, viajes, origenB)
  | igualAlOrigen = validaMasEscalas (destinoA, viajes, origen)
  | otherwise = validaMasEscalas (destinoA, viajes, origenB)
  where
    igualAlDestino = destinoA == origen
    igualAlOrigen = origenB == destino


validaMasEscalas2 :: [AgenciaDeViajes] -> Bool
validaMasEscalas2 ([]:_:_: conjunto) = False
validaMasEscalas2 (_:_:[]: conjunto) = False
validaMasEscalas2 (((origenA , destinoA, tA) : origenIgual) : [] : ((origenB, destinoB, tB) : destinoIgual): conjunto)
  | destinoA == origenB = True
  | otherwise = validaConSiguienteDestino || validaConSiguienteOrigen
  where
    validaConSiguienteDestino = validaMasEscalas2 (((origenA , destinoA, tA) : origenIgual) : [] : destinoIgual: conjunto)
    validaConSiguienteOrigen = validaMasEscalas2 (origenIgual : [] : ((origenB, destinoB, tB) : destinoIgual): conjunto)

validaMasEscalas2 (((origenA , destinoA, tA) : origenIgual) : ((origen, destino, t): diferenteTodo) : ((origenB, destinoB, tB) : destinoIgual) : conjunto) 
  | destinoA == origenB = True
  | igualAlDestino && igualAlOrigen = True
  | otherwise = validaConSiguienteDestinoDif || validaConSiguienteDestinoIgual || validaconSigueinteOrigenIgual || validaConSiguientesParecidos
  where 
    igualAlDestino = destinoA == origen
    igualAlOrigen = origenB == destino
    validaConSiguienteDestinoDif = validaMasEscalas2 (((origenA,destinoA,tA): origenIgual) : diferenteTodo : ((origenB,destinoB,tB):destinoIgual) : conjunto)
    validaConSiguienteDestinoIgual = validaMasEscalas2 (((origenA,destinoA,tA): origenIgual) : ((origen, destino,t):diferenteTodo) : destinoIgual : conjunto)
    validaconSigueinteOrigenIgual = validaMasEscalas2 ( origenIgual : ((origen, destino,t): diferenteTodo) : ((origenB, destinoB, tB) : destinoIgual) : conjunto) 
    validaConSiguientesParecidos = validaMasEscalas2 [conMismoOrigen ((origen, destino,t): diferenteTodo) destinoA, diferenteA2Ciudades ((origen, destino,t): diferenteTodo) destinoA origenB, conMismoDestino ((origen, destino,t): diferenteTodo) origenB]

