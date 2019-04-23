import Data.List
import System.Random
import Data.Time.Clock
import System.IO.Unsafe

data Pirata = Pirata
  { nombrePirata :: String
  , botin        :: [Tesoro]
  } deriving (Show, Eq)

data Tesoro = Tesoro
  { nombreTesoro :: String
  , valor        :: Integer
  } deriving (Show, Eq)

data Barco = Barco
   { tripulacion :: [Pirata]
   , nombreBarco    :: String
   } deriving (Show, Eq)

data Isla = Isla
   { elemento_tipico :: Tesoro
   , nombreIsla    :: String
   } deriving (Show, Eq)

data Ciudad = Ciudad
    { nombreCiudad :: String
    , tesorosSaqueables :: [Tesoro] 
    } deriving (Show, Eq)

--TESOROS
auricularesChetos :: Tesoro
auricularesChetos =
  Tesoro {nombreTesoro = "Auriculares Shure SRH 440", valor = 6000}

zapatillasViotti :: Tesoro
zapatillasViotti = Tesoro {nombreTesoro = "Zapatillas Mike", valor = 400}

zapatillasDini :: Tesoro
zapatillasDini = Tesoro {nombreTesoro = "Zapatillas Mike", valor = 500}

biciCopada :: Tesoro
biciCopada = Tesoro {nombreTesoro = "Bicicleta GT Avalanche", valor = 25000}

brujula :: Tesoro
brujula = Tesoro {nombreTesoro = "Brujula", valor = 10000}

frascoJack :: Tesoro
frascoJack = Tesoro {nombreTesoro = "Frasco de arena", valor = 0}

frascoAnne :: Tesoro
frascoAnne = Tesoro {nombreTesoro = "Frasco de arena", valor = 1}

cajitaMusical :: Tesoro
cajitaMusical = Tesoro {nombreTesoro = "Cajita Musical", valor = 1}

doblones :: Tesoro
doblones = Tesoro {nombreTesoro = "Doblones", valor = 100}

moneda :: Tesoro
moneda = Tesoro {nombreTesoro = "Moneda del cofre muerto", valor = 100}

espada :: Tesoro
espada = Tesoro {nombreTesoro = "Espada de hierro", valor = 50}

cuchillo :: Tesoro
cuchillo = Tesoro {nombreTesoro = "Cuchillo", valor = 5}

oro :: Tesoro
oro = Tesoro {nombreTesoro = "Oro", valor = 75000}

ron :: Tesoro
ron = Tesoro {nombreTesoro = "Ron", valor = 25}

media_sucia :: Tesoro
media_sucia = Tesoro {nombreTesoro = "Media sucia", valor = 10}

--PIRATAS
viotti :: Pirata
viotti =
  Pirata
    { nombrePirata = "Viotti el terrible"
    , botin = [auricularesChetos, zapatillasViotti]
    }

dini :: Pirata
dini =
  Pirata
    {nombrePirata = "Dini el magnifico", botin = [biciCopada, zapatillasDini]}

jackSparrow :: Pirata
jackSparrow =
  Pirata {nombrePirata = "Jack Sparrow", botin = [brujula, frascoJack]}

davidJones :: Pirata
davidJones = Pirata {nombrePirata = "David Jones", botin = [cajitaMusical]}

anneBonny :: Pirata
anneBonny = Pirata {nombrePirata = "Anne Bonny", botin = [doblones, frascoAnne]}

elizabethSwann :: Pirata
elizabethSwann = Pirata {nombrePirata = "Elizabeth Swann", botin = [moneda, espada]}

piratas :: [Pirata]
piratas = [viotti, dini, jackSparrow, davidJones, anneBonny, elizabethSwann]

--BARCOS
perla = Barco { tripulacion = [jackSparrow, anneBonny] , nombreBarco = "Perla Negra"}
holandes = Barco { tripulacion = [davidJones]  , nombreBarco = "Holandes Errante"}

--ISLAS
isla_tortuga = Isla { elemento_tipico = frascoAnne, nombreIsla = "Isla Tortuga" }
isla_ron = Isla { elemento_tipico = ron, nombreIsla = "Isla del Ron" }

islas :: [Isla]
islas = [isla_tortuga, isla_ron]

--CIUDADES
port_royal = Ciudad { nombreCiudad = "Port Royal", tesorosSaqueables = [brujula] }
new_providence = Ciudad { nombreCiudad = "Nueva Providencia", tesorosSaqueables = [oro, ron, doblones] }

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
  nombreTesoro tesoro_1 == nombreTesoro tesoro_2

comparar_valores_tesoros :: Tesoro -> Tesoro -> Bool
comparar_valores_tesoros tesoro_1 tesoro_2 = valor tesoro_1 /= valor tesoro_2

comparar_valores_de_nombres_iguales :: Tesoro -> Tesoro -> Bool
comparar_valores_de_nombres_iguales tesoro_1 tesoro_2 =
  comparar_nombres_tesoros tesoro_1 tesoro_2 &&
  comparar_valores_tesoros tesoro_1 tesoro_2

cumpleCondicion :: [Tesoro] -> Tesoro -> Bool
cumpleCondicion botin tesoro =
  any (comparar_valores_de_nombres_iguales tesoro) botin

tienen_mismo_tesoro_y_valor_diferente :: Pirata -> Pirata -> Bool
tienen_mismo_tesoro_y_valor_diferente pirata =
  any (cumpleCondicion (botin pirata)) . botin

valor_tesoro_mas_valioso :: Pirata -> Integer
valor_tesoro_mas_valioso = maximum . valores_tesoros

adquirir_tesoro :: Pirata -> Tesoro -> Pirata
adquirir_tesoro pirata tesoro =
  Pirata (nombrePirata pirata) (tesoro : (botin pirata))

perder_tesoros_valiosos :: Pirata -> Pirata
perder_tesoros_valiosos pirata =
  Pirata (nombrePirata pirata) (filter ((< 100) . valor) (botin pirata))

perder_tesoros_con_nombre :: String -> Pirata -> Pirata
perder_tesoros_con_nombre nombre pirata =
  Pirata
    (nombrePirata pirata)
    (filter ((/= nombre) . nombreTesoro) (botin pirata))

--TEMPORADA DE SAQUEOS
saquear :: Pirata -> (Tesoro -> Bool) -> Tesoro -> Pirata
saquear pirata forma_saqueo tesoro 
  | forma_saqueo tesoro = adquirir_tesoro pirata tesoro
  | otherwise = pirata

solo_tesoros_valiosos :: Tesoro -> Bool 
solo_tesoros_valiosos = (>100) . valor

solo_tesoros_especificos :: String -> Tesoro -> Bool 
solo_tesoros_especificos clave = (==clave) . nombreTesoro 

pirata_con_corazon :: Tesoro -> Bool 
pirata_con_corazon tesoro = False

--Nota: condicion de cumplimiento del any
evaluar :: Tesoro -> (Tesoro -> Bool) -> Bool
evaluar tesoro forma = forma tesoro

forma_compleja :: [(Tesoro -> Bool)] -> Tesoro -> Bool
forma_compleja formas tesoro = any (evaluar tesoro) formas


-- NAVEGANDO LOS SIETE MARES
incorporar_a_tripulacion :: Pirata -> Barco -> Barco
incorporar_a_tripulacion pirata barco = Barco ((tripulacion barco) ++ [pirata]) (nombreBarco barco)

abandonar_tripulacion :: Pirata -> Barco -> Barco
abandonar_tripulacion pirata barco =  Barco (delete pirata (tripulacion barco)) (nombreBarco barco)

anclar_en_isla :: Barco -> Isla -> Barco
anclar_en_isla barco isla = Barco (tomar_tesoros (tripulacion barco) (elemento_tipico isla)) (nombreBarco barco)

tomar_tesoros :: [Pirata] -> Tesoro -> [Pirata]
tomar_tesoros tripulacion tesoro = map (flip adquirir_tesoro tesoro) tripulacion

--Nota: cada barco tiene una forma definida de saquear
atacar_ciudad :: Ciudad -> Barco -> (Tesoro -> Bool) -> Barco
atacar_ciudad ciudad barco forma_saqueo = Barco (repartir_tesoros (tripulacion barco) forma_saqueo (tesorosSaqueables ciudad)) (nombreBarco barco)

repartir_tesoros :: [Pirata] -> (Tesoro -> Bool) -> [Tesoro] -> [Pirata]
repartir_tesoros (pirata:piratas) forma_saqueo (tesoro:tesoros) = saquear pirata forma_saqueo tesoro : repartir_tesoros piratas forma_saqueo tesoros
--condicion corte cuando hay mas tesoros que piratas
repartir_tesoros [] forma_saqueo (tesoro:tesoros) = [] 
--condicion de corte cuando hay mas piratas que tesoros 
repartir_tesoros (pirata:piratas) forma_saqueo [] = []
--condicion de corte si hay igual cantidad de piratas que tesoros
repartir_tesoros [] forma_saqueo [] = []




--HISTORIA
comenzar_historia :: IO (String) 
comenzar_historia = do
    putStrLn("Ahoy novato! Estas aqui para convertirte en un nuevo poderoso pirata!")
    putStrLn("Es hora de comenzar tu aventura! Pero antes...")
    protagonista <- crear_pirata
    putStrLn("Ahora es hora de salir a navegar los 7 mares! Tu historia comienza en la isla Tortuga.")
    resultado_historia <- menu_historia protagonista
    return resultado_historia

crear_pirata :: IO (Pirata)
crear_pirata = do    
    putStrLn("Cual es tu nombre?")
    nombreProtagonista <- getLine
    putStrLn("Bienvenido " ++ nombreProtagonista ++ "!")
    putStrLn("Ahora que ya tienes un nombre, solo te hace falta un botin...")
    putStrLn("... tienes un botin, verdad?")
    putStrLn("Agh... no tienes botin... bueno, entonces tu botin inicial consiste en una media sucia.")
    putStrLn("Si te quedas sin botin, seras expulsado de la vida pirata!")
    putStrLn("Y recuerda que el valor de tus tesoros determina tu fortaleza en combate.")
    return Pirata {nombrePirata = nombreProtagonista, botin = [media_sucia]}

menu_historia :: Pirata -> IO String
menu_historia protagonista = do
    putStrLn("Que deseas realizar a continuacion?")
    putStrLn("(1)-ROBAR UN BARCO")
    putStrLn("(2)-SAQUEAR CIUDAD")
    putStrLn("(3)-RETIRARSE DE LA PIRATERIA")
    opcion <- getLine
    desarrollar_historia (read opcion :: Integer) protagonista 

menu_historia_con_barco :: Barco -> IO String
menu_historia_con_barco barco = do
    putStrLn("Que deseas realizar a continuacion?")
    putStrLn("(1)-ATACAR UN BARCO")
    putStrLn("(2)-ANCLAR EN UNA ISLA CERCANA")
    putStrLn("(3)-ATACAR UNA CIUDAD")
    putStrLn("(4)-RETIRARSE DE LA PIRATERIA")
    opcion <- getLine
    desarrollar_historia_en_barco (read opcion :: Integer) barco

desarrollar_historia :: Integer -> Pirata -> IO String 
desarrollar_historia opcion protagonista = case opcion of
     1 -> robar_barco protagonista
--     2 -> saquear_ciudad protagonista
--     3 -> retirarse protagonista
     _ -> menu_historia protagonista

desarrollar_historia_en_barco :: Integer -> Barco -> IO String
desarrollar_historia_en_barco opcion barco = case opcion of
--    1 -> atacar_barco protagonista
  2 -> anclar_en_isla_cercana barco 
--    3 -> atacar_ciudad barco
--    4 -> retirarse protagonista
  _ -> desarrollar_historia_en_barco opcion barco


-- ACCIONES POSIBLES

robar_barco :: Pirata -> IO String
robar_barco protagonista = do
    putStrLn("Solo hay un velero triste anclado en el muelle...")
    putStrLn("Pero... que va, lo tomas y zarpas. Enhorabuena! Ahora es tuyo. Que nombre le pondras?")
    nombreVelero <- getLine
    menu_historia_con_barco Barco { tripulacion = [protagonista] , nombreBarco = nombreVelero}

anclar_en_isla_cercana :: Barco -> IO String
anclar_en_isla_cercana barco = do
    putStrLn("Los vientos de los siete mares te arrastran hacia la isla mas cercana.")
    putStrLn("En el horizonte se vislumbra el contorno de la isla " ++ (nombreIsla (unsafePerformIO islaAleatoria)))
    return "fin"



--- FUNCIONES AUXILIARES

islaAleatoria :: IO Isla 
islaAleatoria =  valorAleatorio islas

valorAleatorio :: [a] -> IO a
valorAleatorio list = do
    i <- getStdRandom (randomR (0, length list - 1)) 
    return $ list !! i

confirmar :: String -> IO Bool
confirmar mensaje_a_confirmar = do
    putStrLn (mensaje_a_confirmar)
    putStrLn ("(s)i/(n)o")
    confirmacion <- getLine
    procesar_confirmacion confirmacion

procesar_confirmacion :: String -> IO Bool
procesar_confirmacion confirmacion 
  | elem confirmacion ["s", "S"] = return True
  | elem confirmacion ["n", "N"] = return False
  | otherwise = confirmar "Ha ingresado una opcion incorrecta. Por favor, ingrese"
