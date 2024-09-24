module Prueba where

import Solucion (AgenciaDeViajes, Ciudad)

pruebaIdaYVuelta :: AgenciaDeViajes -> Ciudad -> Bool
pruebaIdaYVuelta viajes ciudad = sacaOrigenYDestino [conMismoOrigen viajes ciudad, diferenteACiudad viajes ciudad, conMismoDestino viajes ciudad] ciudad

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

sacaOrigenYDestino :: [AgenciaDeViajes] -> Ciudad -> Bool
sacaOrigenYDestino ([] : _ : _ : _) ciudad = False
sacaOrigenYDestino (_ : _ : [] : _) ciudad = False
sacaOrigenYDestino (((origen1, destino1, t1) : origenIgual) : diferenteTodo : ((origen3, destino3, t3) : destinoIgual) : conjunto) ciudad
  | unaEscala = True
  | masEscalas && esOrigen1Ydestino3 = validaMasEscalas (destino1, diferenteTodo, origen3) || sacaOrigenYDestino (((origen1, destino1, t1) : origenIgual) : diferenteTodo : destinoIgual : conjunto) ciudad || sacaOrigenYDestino (origenIgual : diferenteTodo : ((origen3, destino3, t3) : destinoIgual) : conjunto) ciudad
  | otherwise = validaMasEscalas (destino3, diferenteTodo, origen1) || sacaOrigenYDestino (((origen1, destino1, t1) : origenIgual) : diferenteTodo : destinoIgual : conjunto) ciudad || sacaOrigenYDestino (origenIgual : diferenteTodo : ((origen3, destino3, t3) : destinoIgual) : conjunto) ciudad
  where
    unaEscala = origen1 /= origen3 && (origen1 == ciudad || origen3 == ciudad) && destino1 /= destino3 && (destino1 == ciudad || destino3 == ciudad) && (destino1 == origen3 || destino3 == origen1)
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


-- validaMasEscalas2 :: (Ciudad, AgenciaDeViajes, Ciudad) -> Bool

