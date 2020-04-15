
module Auxiliares where

import Piratas_Caribe
import Control.Concurrent
import Data.Char (toLower, toUpper)
import Data.List as List
import Data.Time.Clock
import System.Random
import System.IO.Unsafe

--- FUNCIONES AUXILIARES

barco_con_protagonista_modificado :: Pirata -> Barco -> Barco
barco_con_protagonista_modificado protagonista barco = barco { tripulacion = protagonista:(List.drop 1 (tripulacion barco))} 

mock_end :: IO String
mock_end = return "End"

perder_tesoros_aleatorios :: Pirata -> Pirata
perder_tesoros_aleatorios pirata = pirata {botin = unsafePerformIO (tesoros_aleatorios (botin pirata))}

perder_tripulantes_protagonista :: Barco -> Barco
perder_tripulantes_protagonista barco = barco {tripulacion = (get_protagonista barco) : (unsafePerformIO(tripulantes_aleatorios (tail (tripulacion barco))))}

perder_tripulantes :: Barco -> Barco
perder_tripulantes barco = barco {tripulacion = ( unsafePerformIO(tripulantes_aleatorios (tripulacion barco)))}

incorporar_tripulantes :: Barco -> [Pirata] -> Barco 
incorporar_tripulantes barco tripulacion_obtenida = barco { tripulacion = (tripulacion barco) ++ tripulacion_obtenida}

get_protagonista :: Barco -> Pirata
get_protagonista barco = head (tripulacion barco)

entregar_tesoro :: Tesoro -> Pirata -> Pirata
entregar_tesoro tesoro_a_entregar pirata = pirata {botin = List.delete tesoro_a_entregar (botin pirata)} 

quedar_con_algunos_tesoros :: Pirata -> Pirata
quedar_con_algunos_tesoros pirata = pirata {botin = unsafePerformIO (tesoros_aleatorios (botin pirata))}

adquirir_tesoro_aleatorio :: Pirata -> [Tesoro] -> Pirata
adquirir_tesoro_aleatorio pirata tesoros_saqueables  = adquirir_tesoro pirata (unsafePerformIO (tesoro_aleatorio tesoros_saqueables))

adquirir_tesoro_mas_valioso :: Pirata -> [Tesoro] -> Pirata
adquirir_tesoro_mas_valioso pirata tesoros_adversario 
  | length tesoros_adversario > 0 = adquirir_tesoro pirata (tesoro_mas_valioso tesoros_adversario)
  | otherwise = pirata

tesoros_tripulantes :: Barco -> [Tesoro]
tesoros_tripulantes barco = concatMap botin (tripulacion barco)

tesoro_aleatorio :: [Tesoro] -> IO Tesoro 
tesoro_aleatorio tesoros = valor_aleatorio tesoros

tesoros_aleatorios :: [Tesoro] -> IO [Tesoro] 
tesoros_aleatorios tesoros = valores_aleatorios tesoros

isla_aleatoria :: IO Isla 
isla_aleatoria =  valor_aleatorio islas

barco_aleatorio :: IO Barco
barco_aleatorio = valor_aleatorio barcos

tripulante_aleatorio :: [Pirata] -> IO Pirata
tripulante_aleatorio tripulacion = valor_aleatorio tripulacion

tripulantes_aleatorios :: [Pirata] -> IO [Pirata]
tripulantes_aleatorios tripulacion = valores_aleatorios tripulacion

contrincante_aleatorio :: [Barco] -> IO Barco
contrincante_aleatorio contrincantes = valor_aleatorio contrincantes

obtener_nuevo_tripulante :: Barco -> IO Pirata
obtener_nuevo_tripulante barco = 
    valor_aleatorio (obtener_grupo_sin_subgrupo piratas (tripulacion barco))

obtener_grupo_sin_subgrupo :: Ord a => [a] -> [a] -> [a]
obtener_grupo_sin_subgrupo grupo subgrupo = grupo List.\\ subgrupo

fuerza_golpe_protagonista :: Pirata -> IO Integer
fuerza_golpe_protagonista protagonista = do
    let fuerza_golpe = sum (valores_tesoros protagonista)
    putStrLn("Desenvainas tu espada y preparas tu golpe con una fuerza de " ++ show(fuerza_golpe))
    return fuerza_golpe
    
fuerza_golpe_contrincante :: Pirata -> IO Integer
fuerza_golpe_contrincante pirata = do
    let fuerza_base_contrincante = valor (unsafePerformIO (tesoro_aleatorio (botin pirata)))
    let fuerza_guardia = fromInteger (fuerza_base_contrincante) * coeficiente_combate pirata
    putStrLn("En respuesta, tu rival golpea con una fuerza de " ++ show (floor fuerza_guardia))
    return (floor fuerza_guardia)

coeficiente_combate :: Pirata -> Float
coeficiente_combate pirata =  2 ^^ ((length (botin pirata))-3)

valor_aleatorio :: [a] -> IO a
valor_aleatorio list = do
    i <- getStdRandom (randomR (0, length list - 1))
    return $ list !! i

valores_aleatorios :: [a] -> IO [a]
valores_aleatorios list = do
    i <- getStdRandom (randomR (0, length list)) 
    return $ List.take i list 

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
  | otherwise = confirmar "Has ingresado una opción incorrecta. Por favor, ingrese"

estado_piratas :: [Pirata] -> IO() 
estado_piratas lista_piratas = do
    putStrLn (unlines $ map show [pirata | pirata <- lista_piratas])

cantidad_tesoros_valiosos :: Pirata -> Int 
cantidad_tesoros_valiosos pirata =
   length (List.filter (es_valioso) (botin pirata))

suspenso :: Int -> IO () 
suspenso segundos = threadDelay (1000000 * segundos)

--- NOMBRES DE TESOROS - PLURAL

nombre_plural :: Tesoro -> String
nombre_plural tesoro 
  | tesoro == biciCopada = "bicicletas GT Avalanche"
  | tesoro == brujula = "brujulas"
  | tesoro == frascoJack = "frascos de arena"
  | tesoro == frascoAnne = "frascos de arena"
  | tesoro == moneda = "monedas"
  | tesoro == espada = "espadas"
  | tesoro == cuchillo = "cuchillos"
  | tesoro == media_sucia = "medias sucias"
  | otherwise =  nombre_en_minusculas tesoro

--- NOMBRES DE TESOROS - CON ARTICULOS

una = [biciCopada, brujula, cajitaMusical, moneda, espada, media_sucia]
unas = [zapatillasMike, zapatillasArdidas]
un = [frascoJack, frascoAnne, cuchillo]
unos = [doblones]

nombre_con_articulo :: Tesoro -> String
nombre_con_articulo tesoro
  | elem tesoro unos = "unos " ++ nombre_en_minusculas tesoro
  | elem tesoro unas  = "unas " ++ nombre_en_minusculas tesoro
  | elem tesoro una = "una " ++ nombre_en_minusculas tesoro
  | elem tesoro un = "un " ++ nombre_en_minusculas tesoro
  | otherwise = nombre_en_minusculas tesoro

nombre_en_minusculas :: Tesoro -> String
nombre_en_minusculas tesoro = lowerString (nombre_tesoro tesoro)

lowerString :: String -> String
lowerString = List.map toLower

upperString :: String->String
upperString = List.map toUpper
