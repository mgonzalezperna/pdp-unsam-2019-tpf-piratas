module Piratas_Caribe where

import Data.List
import Data.Function (on)

data Pirata = Pirata
  { nombre_pirata :: String
  , botin        :: [Tesoro]
  } deriving (Ord, Eq)

instance Show Pirata where
  show pirata = nombre_pirata pirata ++ "\n\nBotin: " ++ show (botin pirata) ++ "\n\n"

data Tesoro = Tesoro
  { nombre_tesoro :: String
  , valor        :: Integer
  } deriving (Ord, Eq)

instance Show Tesoro where
  show tesoro = " " ++ nombre_tesoro tesoro ++ ": $" ++ show (valor tesoro) ++ " " 

data Barco = Barco
   { tripulacion :: [Pirata]
   , nombre_barco    :: String
   } deriving (Show, Eq)

data Isla = Isla
   { elemento_tipico :: Tesoro
   , nombre_isla    :: String
   } deriving (Show, Eq)

data Ciudad = Ciudad
    { nombre_ciudad :: String
    , tesoros_saqueables :: [Tesoro] 
    } deriving (Show, Eq)

--TESOROS
auricularesChetos :: Tesoro
auricularesChetos =
  Tesoro {nombre_tesoro = "Auriculares Shure SRH 440", valor = 6000}

zapatillasMike :: Tesoro
zapatillasMike = Tesoro {nombre_tesoro = "Zapatillas Mike", valor = 400}

zapatillasArdidas :: Tesoro
zapatillasArdidas = Tesoro {nombre_tesoro = "Zapatillas Ardidas", valor = 500}

biciCopada :: Tesoro
biciCopada = Tesoro {nombre_tesoro = "Bicicleta GT Avalanche", valor = 25000}

brujula :: Tesoro
brujula = Tesoro {nombre_tesoro = "Brujula", valor = 10000}

frascoJack :: Tesoro
frascoJack = Tesoro {nombre_tesoro = "Frasco de arena", valor = 0}

frascoAnne :: Tesoro
frascoAnne = Tesoro {nombre_tesoro = "Frasco de arena", valor = 1}

cajitaMusical :: Tesoro
cajitaMusical = Tesoro {nombre_tesoro = "Cajita Musical", valor = 1}

doblones :: Tesoro
doblones = Tesoro {nombre_tesoro = "Doblones", valor = 100}

moneda :: Tesoro
moneda = Tesoro {nombre_tesoro = "Moneda del cofre muerto", valor = 100}

espada :: Tesoro
espada = Tesoro {nombre_tesoro = "Espada de hierro", valor = 50}

cuchillo :: Tesoro
cuchillo = Tesoro {nombre_tesoro = "Cuchillo", valor = 5}

oro :: Tesoro
oro = Tesoro {nombre_tesoro = "Oro", valor = 75000}

ron :: Tesoro
ron = Tesoro {nombre_tesoro = "Ron", valor = 25}

media_sucia :: Tesoro
media_sucia = Tesoro {nombre_tesoro = "Media sucia", valor = 1}

--PIRATAS
viotti :: Pirata
viotti =
  Pirata
    { nombre_pirata = "Viotti el terrible"
    , botin = [auricularesChetos, zapatillasMike]
    }

dini :: Pirata
dini =
  Pirata
    {nombre_pirata = "Dini el magnifico", botin = [biciCopada, zapatillasArdidas]}

jackSparrow :: Pirata
jackSparrow =
  Pirata {nombre_pirata = "Jack Sparrow", botin = [brujula, frascoJack]}

davidJones :: Pirata
davidJones = Pirata {nombre_pirata = "David Jones", botin = [cajitaMusical]}

anneBonny :: Pirata
anneBonny = Pirata {nombre_pirata = "Anne Bonny", botin = [doblones, frascoAnne]}

elizabethSwann :: Pirata
elizabethSwann = Pirata {nombre_pirata = "Elizabeth Swann", botin = [moneda, espada]}

piratas :: [Pirata]
piratas = [viotti, dini, jackSparrow, davidJones, anneBonny, elizabethSwann]

--BARCOS
perla = Barco { tripulacion = [jackSparrow, anneBonny] , nombre_barco = "Perla Negra"}
holandes = Barco { tripulacion = [davidJones]  , nombre_barco = "Holandes Errante"}

barcos :: [Barco]
barcos = [perla, holandes]

--ISLAS
isla_tortuga = Isla { elemento_tipico = frascoAnne, nombre_isla = "Isla Tortuga" }
isla_ron = Isla { elemento_tipico = ron, nombre_isla = "Isla del Ron" }

islas :: [Isla]
islas = [isla_tortuga, isla_ron]

--CIUDADES
port_royal = Ciudad { nombre_ciudad = "Port Royal", tesoros_saqueables = [brujula] }
new_providence = Ciudad { nombre_ciudad = "Nueva Providencia", tesoros_saqueables = [oro, ron, doblones] }

ciudades :: [Ciudad]
ciudades = [port_royal, new_providence]

--TESOROS PIRATAS
cantidad_tesoros :: Pirata -> Int
cantidad_tesoros pirata = length (botin pirata)

es_afortunado :: Pirata -> Bool
es_afortunado = (> 10000) . sum . valores_tesoros

valores_tesoros :: Pirata -> [Integer]
valores_tesoros pirata = map valor (botin pirata)

comparar_nombres_tesoros :: Tesoro -> Tesoro -> Bool
comparar_nombres_tesoros tesoro_1 tesoro_2 =
  nombre_tesoro tesoro_1 == nombre_tesoro tesoro_2

comparar_valores_tesoros :: Tesoro -> Tesoro -> Bool
comparar_valores_tesoros tesoro_1 tesoro_2 = valor tesoro_1 /= valor tesoro_2

comparar_valores_de_nombres_iguales :: Tesoro -> Tesoro -> Bool
comparar_valores_de_nombres_iguales tesoro_1 tesoro_2 =
  comparar_nombres_tesoros tesoro_1 tesoro_2 &&
  comparar_valores_tesoros tesoro_1 tesoro_2

cumple_condicion :: [Tesoro] -> Tesoro -> Bool
cumple_condicion botin tesoro =
  any (comparar_valores_de_nombres_iguales tesoro) botin

tienen_mismo_tesoro_y_valor_diferente :: Pirata -> Pirata -> Bool
tienen_mismo_tesoro_y_valor_diferente pirata =
  any (cumple_condicion (botin pirata)) . botin

valor_tesoro_mas_valioso :: Pirata -> Integer
valor_tesoro_mas_valioso = maximum . valores_tesoros

adquirir_tesoro :: Pirata -> Tesoro -> Pirata
adquirir_tesoro pirata tesoro =
  Pirata (nombre_pirata pirata) (tesoro : (botin pirata))

perder_tesoros_valiosos :: Pirata -> Pirata
perder_tesoros_valiosos pirata =
  Pirata (nombre_pirata pirata) (filter (not . es_valioso) (botin pirata))

perder_tesoros_con_nombre :: String -> Pirata -> Pirata
perder_tesoros_con_nombre nombre pirata =
  Pirata
    (nombre_pirata pirata)
    (filter ((/= nombre) . nombre_tesoro) (botin pirata))

es_valioso :: Tesoro -> Bool
es_valioso = (>= 100) . valor

tesoro_mas_valioso :: [Tesoro] -> Tesoro
tesoro_mas_valioso tesoros = maximumBy (compare `on` valor) tesoros

--TEMPORADA DE SAQUEOS
saquear :: Pirata -> (Tesoro -> Bool) -> Tesoro -> Pirata
saquear pirata forma_saqueo tesoro 
  | forma_saqueo tesoro = adquirir_tesoro pirata tesoro
  | otherwise = pirata

solo_tesoros_valiosos :: Tesoro -> Bool 
solo_tesoros_valiosos = (>100) . valor

solo_tesoros_especificos :: String -> Tesoro -> Bool 
solo_tesoros_especificos clave = (==clave) . nombre_tesoro 

pirata_con_corazon :: Tesoro -> Bool 
pirata_con_corazon tesoro = False

--Nota: condicion de cumplimiento del any
evaluar :: Tesoro -> (Tesoro -> Bool) -> Bool
evaluar tesoro forma = forma tesoro

forma_compleja :: [(Tesoro -> Bool)] -> Tesoro -> Bool
forma_compleja formas tesoro = any (evaluar tesoro) formas


-- NAVEGANDO LOS SIETE MARES
incorporar_a_tripulacion :: Pirata -> Barco -> Barco
incorporar_a_tripulacion pirata barco = Barco ((tripulacion barco) ++ [pirata]) (nombre_barco barco)

abandonar_tripulacion :: Pirata -> Barco -> Barco
abandonar_tripulacion pirata barco =  Barco (delete pirata (tripulacion barco)) (nombre_barco barco)

anclar_en_isla :: Barco -> Isla -> Barco
anclar_en_isla barco isla = Barco (tomar_tesoros (tripulacion barco) (elemento_tipico isla)) (nombre_barco barco)

tomar_tesoros :: [Pirata] -> Tesoro -> [Pirata]
tomar_tesoros tripulacion tesoro = map (flip adquirir_tesoro tesoro) tripulacion

--Nota: cada barco tiene una forma definida de saquear
atacar_ciudad :: Ciudad -> Barco -> (Tesoro -> Bool) -> Barco
atacar_ciudad ciudad barco forma_saqueo = Barco (repartir_tesoros (tripulacion barco) forma_saqueo (tesoros_saqueables ciudad)) (nombre_barco barco)

repartir_tesoros :: [Pirata] -> (Tesoro -> Bool) -> [Tesoro] -> [Pirata]
repartir_tesoros (pirata:piratas) forma_saqueo (tesoro:tesoros) = saquear pirata forma_saqueo tesoro : repartir_tesoros piratas forma_saqueo tesoros
--condicion corte cuando hay mas tesoros que piratas
repartir_tesoros [] forma_saqueo (tesoro:tesoros) = [] 
--condicion de corte cuando hay mas piratas que tesoros 
repartir_tesoros (pirata:piratas) forma_saqueo [] = []
--condicion de corte si hay igual cantidad de piratas que tesoros
repartir_tesoros [] forma_saqueo [] = []
