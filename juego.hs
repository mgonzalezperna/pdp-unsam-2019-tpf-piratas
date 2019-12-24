
module Juego where

import Piratas_Caribe
import System.Random
import Data.Time.Clock
import System.IO.Unsafe
import Control.Concurrent
import Data.Char (toLower)
import Data.List

--HISTORIA
comenzar_historia :: IO () 
comenzar_historia = do
    putStrLn("Ahoy novato! Estas aquí para convertirte en un poderoso pirata!")
    putStrLn("Es hora de comenzar tu aventura! Pero antes...")
    protagonista <- crear_pirata
    putStrLn("Ahora es hora de salir a navegar los 7 mares! Tu historia comienza en la isla Tortuga.")
    resultado_historia <- menu_historia protagonista
    putStrLn(resultado_historia)
    putStrLn("Fin.")

crear_pirata :: IO (Pirata)
crear_pirata = do    
    putStrLn("¿Cuál es tu nombre?")
    nombreProtagonista <- getLine
    putStrLn("Bienvenido " ++ nombreProtagonista ++ "!")
    putStrLn("Ahora que ya tienes un nombre, sólo te hace falta un botín...")
    putStrLn("... ¿tienes un botín, verdad?")
    putStrLn("Agh... bueno, en este caso, tu botín inicial consiste en... una media sucia!")
    putStrLn("Tomala!")
    putStrLn("Si te quedas sin botín, serás expulsado de la vida pirata!")
    putStrLn("Y recuerda que el valor de tus tesoros determina tu fortaleza en combate.")
    putStrLn("\n")
    return Pirata {nombrePirata = nombreProtagonista, botin = [media_sucia]}

menu_historia :: Pirata -> IO String
menu_historia protagonista = do
    putStrLn("¿Qué deseas realizar a continuación?")
    putStrLn("(1)-ROBAR UN BARCO")
    putStrLn("(2)-SAQUEAR CIUDAD")
    putStrLn("(3)-RETIRARSE DE LA PIRATERIA")
    putStrLn("(4)-VER MI ESTADO")
    opcion <- getLine
    desarrollar_historia opcion protagonista 

menu_historia_con_barco :: Barco -> IO String
menu_historia_con_barco barco = do
    putStrLn("¿Qué deseas realizar a continuación?")
    putStrLn("(1)-ATACAR UN BARCO")
    putStrLn("(2)-ANCLAR EN UNA ISLA CERCANA")
    putStrLn("(3)-ATACAR UNA CIUDAD")
    putStrLn("(4)-RETIRARSE DE LA PIRATERIA")
    putStrLn("(5)-VER MI ESTADO")
    opcion <- getLine
    desarrollar_historia_en_barco opcion barco

desarrollar_historia :: String -> Pirata -> IO String 
desarrollar_historia opcion protagonista = case opcion of
     "1" -> robar_barco protagonista
     "2" -> elegir_ciudad_a_saquear protagonista
--     3 -> retirarse protagonista
     "4" ->ver_estado protagonista menu_historia protagonista
     _ -> menu_historia protagonista

desarrollar_historia_en_barco :: String -> Barco -> IO String
desarrollar_historia_en_barco opcion barco = case opcion of
--    1 -> atacar_barco barco 
      "2" -> anclar_en_isla_cercana barco 
      "3" -> elegir_ciudad_a_asediar barco
--    4 -> retirarse protagonista
      "5" -> ver_estado (get_protagonista barco) menu_historia_con_barco barco
      _ -> menu_historia_con_barco barco



-- ACCIONES POSIBLES

robar_barco :: Pirata -> IO String
robar_barco protagonista = do
    putStrLn("Sólo hay un velero triste anclado en el muelle...")
    putStrLn("Pero... que va, lo tomas y zarpas. Enhorabuena! Ahora es tuyo. ¿Qué nombre le pondrás?")
    nombreNuevoBarco <- getLine
    menu_historia_con_barco Barco { tripulacion = [protagonista] , nombreBarco = nombreNuevoBarco}

anclar_en_isla_cercana :: Barco -> IO String
anclar_en_isla_cercana barco = do
    --pirata <- (head (tripulacion barco))
    isla <- islaAleatoria
    putStrLn("Los vientos de los siete mares te arrastran hacia la isla más cercana.")
    putStrLn("En el horizonte se vislumbra el contorno de la isla " ++ (nombreIsla isla))
    putStrLn("Cuando desbarcan, ven un enorme depósito de " ++ nombre_plural(elemento_tipico isla) ++ ".")
    putStrLn("Añades el tesoro a tu botín y retomas tu aventura, a la espera de que la proxima vez hagas algo más emocionante...")
    menu_historia_con_barco $ anclar_en_isla barco isla


--SAQUEO CIUDAD

elegir_ciudad_a_saquear :: Pirata -> IO String
elegir_ciudad_a_saquear protagonista = do
    putStrLn("\n¿Qué ciudad deseas saquear?")
    putStrLn("(1)-Port Royal") --MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM, MIEDO
    putStrLn("(2)-New Providence")
    eleccion <- getLine
    procesar_eleccion_ciudad eleccion protagonista

saquear_ciudad :: Pirata -> Ciudad -> IO String
saquear_ciudad protagonista ciudad = do
    putStrLn(concat ["Te dirijes con paso firme a la ciudad de " , (nombreCiudad ciudad) , " dispuesto a llevarte todos sus tesoros!!"])
    putStrLn("Pero en la entrada te detienen unos guardias con cara de pocos amigos. Te piden que te retires... ¿Qué decisión tomas?\n")
    putStrLn("(1)-SOBORNAR A LOS GUARDIAS PARA QUE TE DEN ALGUNOS TESOROS")
    putStrLn("(2)-COMBATIR CONTRA LOS GUARDIAS\n")
    eleccion <- getLine
    elegir_tipo_saqueo eleccion protagonista ciudad

sobornar_guardias :: Pirata -> Ciudad -> IO String
sobornar_guardias protagonista ciudad
  | length (botin protagonista) > 0 = soborno_exitoso protagonista ciudad
  | otherwise = soborno_imposible protagonista
    
soborno_exitoso :: Pirata -> Ciudad -> IO String
soborno_exitoso protagonista ciudad = do
    let tesoroAEntregar = unsafePerformIO (tesoroAleatorio (botin protagonista))
    putStrLn("Ofreces uno de tus tesoros a los guardias. \nEllos eligen " ++ nombre_con_articulo tesoroAEntregar)
    putStrLn("Lo entregas y te dan acceso a la boveda de los tesoros a cambio\n")
    menu_historia (realizar_intercambio protagonista tesoroAEntregar (tesorosSaqueables ciudad))

soborno_imposible :: Pirata -> IO String
soborno_imposible protagonista = return "Pero los guardias se dan cuenta que no tienes ni un doblon de oro. No vacilan en absoluto y te encierran en las mazmorras. Tus días de pirata están acabados."

combatir_guardias :: Pirata -> Ciudad -> IO String
combatir_guardias protagonista ciudad = do
    putStrLn("Ningun guardia en todo el caribe puede ser capaz de extorsionarte sin llevarse su merecido!")
    putStrLn("Desenvainas tu espada y te preparas para el combate")
    (resultado_combate_guardias (valorAleatorio [0,1])) protagonista ciudad

resultado_combate_guardias :: IO Int -> (Pirata -> Ciudad -> IO String)
resultado_combate_guardias resultado
  | (unsafePerformIO resultado) == 1 = ganar_combate
  | otherwise = perder_combate

ganar_combate :: Pirata -> Ciudad -> IO String
ganar_combate protagonista ciudad = do
    putStrLn("Con un golpe certero del sable desarmas a tus contrincantes, que huyen atemorizados ante tu habilidad. El tesoro queda a tu merced")
    menu_historia (protagonista { botin = (tesorosSaqueables ciudad) })

perder_combate :: Pirata -> Ciudad -> IO String
perder_combate protagonista ciudad = do
  putStrLn ("Sin embargo, tus enemigos son más habiles de lo que esperabas. Con golpes certeros te desarman y quedas a su merced. No tienes más alternativa que entregar todos tus tesoros valiosos a cambio de que te dejen escapar.")
  evaluar_si_continua_segun_tesoros (perder_tesoros_valiosos protagonista)

evaluar_si_continua_segun_tesoros :: Pirata -> IO String
evaluar_si_continua_segun_tesoros protagonista
  | cantidad_tesoros_valiosos protagonista > 0 = menu_historia protagonista
  | otherwise = encarcelamiento protagonista

encarcelamiento :: Pirata -> IO String
encarcelamiento protagonista = return "Pero... ya no tienes tesoros que entregar! Los guardias te arrastran a la celda mas lejana de todo el calabozo. Tus días de pirata estan acabados. Quizás tengas más suerte la proxima vez."


--DESDE BARCO

elegir_ciudad_a_asediar :: Barco -> IO String
elegir_ciudad_a_asediar barco = do
    putStrLn("\n¿Qué ciudad deseas asediar?")
    putStrLn("(1)-Port Royal") --MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM, MIEDO
    putStrLn("(2)-New Providence")
    eleccion <- getLine
    procesar_eleccion_ciudad_asedio eleccion barco

bombardear_ciudad :: Barco -> Ciudad -> IO String
bombardear_ciudad barco ciudad= do
    putStrLn("El barco se alinea, mientras se preparan los cañones y apuntan al fuerte de la ciudad. Te preparas a dar la voz de mando...")
    suspenso(1)
    putStrLn("PREPAREN")
    suspenso(1)
    putStrLn("APUNTEN")
    suspenso(2)
    putStrLn("FUEGOO!")
    suspenso(1)
    (resultado_asedio (valorAleatorio [0,1])) barco ciudad

resultado_asedio :: IO Int -> (Barco -> Ciudad -> IO String)
resultado_asedio resultado_asedio
  | (unsafePerformIO resultado_asedio) == 1 = invadir_ciudad
  | otherwise = ser_repelidos_por_ciudad

invadir_ciudad :: Barco -> Ciudad -> IO String
invadir_ciudad barco ciudad = do
    putStrLn("Las murallas estallan en pedazos y rapidamente te escabulles dentro del fuerte de la ciudad buscando un botín.")
    botin_adquirido <- tesoroAleatorio( tesorosSaqueables ciudad )
    putStrLn("Los guardias te persiguen por lo que solo eres capaz de tomar " ++ nombre_con_articulo botin_adquirido ++ " antes de volver a embarcar.")
    let protagonista = get_protagonista barco
    let protagonista_con_tesoro = protagonista { botin = botin (protagonista) ++ [botin_adquirido] }
    menu_historia_con_barco (barco { tripulacion = protagonista_con_tesoro:(drop 1 (tripulacion barco))})

ser_repelidos_por_ciudad :: Barco -> Ciudad -> IO String
ser_repelidos_por_ciudad barco ciudad = do
    putStrLn("... pero")
    suspenso(1)
    putStrLn("Nada ha pasado.")
    suspenso(1)
    putStrLn("Quizás hubiese sido buena idea revisar el estado de la polvora antes de atacar...")
    putStrLn("Demasiado tarde! Los cañones del fuerte arrasan con tu nave y de pronto te encuentras escupiendo agua y arena en la playa. El mar te arranco algunos tesoro. Sin embargo, aún puedes asaltar la ciudad a pie...")
    let protagonista = get_protagonista barco
    saquear_ciudad (protagonista { botin = unsafePerformIO (tesorosAleatorios (botin (protagonista))) }) ciudad

--- BATALLA MARINA

encuentro_barco :: Barco -> IO String
encuentro_barco barco = do
    barco_adversario <- barcoAleatorio
    putStrLn("Desde el largavistas alcanzas a vislumbrar la silueta de un barco vulnerable.")
    putStrLn("Es el " ++ nombreBarco barco_adversario ++ "!")
    putStrLn("Te lanzas a su encuentro a toda velocidad, preparando los cañones, listo para abordarlos.")
    batalla_barcos cantidad_de_turnos barco barco_adversario

cantidad_de_turnos :: [Integer]
--list que define la cantidad tope de turnos de la batalla, en este caso, 3.
cantidad_de_turnos = [1,2]

cantidad_de_relatos :: [Integer]
cantidad_de_relatos = [1,2,3]

relatos_de_la_batalla :: IO Integer -> String
relatos_de_la_batalla frase = case unsafePerformIO(frase) of
    1 -> "Trozos de madera cruzan el cielo y se mezclan con las gotas de agua en el fragor de la batalla."
    2 -> "Grandes volutas de humo se alzan sobre el horizonte mientras las bocas de los barcos vomitan llamas y hierro."
    3 -> "Pedazos de ambos barcos llenan el aire y golpean duramente contra las olas."

batalla_barcos :: [Integer] -> Barco -> Barco -> IO String

batalla_barcos [] barco barco_adversario = do
    let contrincantes = [barco, barco_adversario]
    let en_ventaja = delete (barco_mas_daniado barco barco_adversario) contrincantes 
    putStrLn ("Los barcos se alinean de cara a lanzarse a las armas una última vez. Los tripulantes del " ++ nombreBarco (head en_ventaja) ++ " se ven confiados mientras se preparan para el abordaje.")
    putStrLn (relatos_de_la_batalla (valorAleatorio cantidad_de_relatos))
    mock_end

batalla_barcos (head:[]) barco barco_adversario = do
    let barco_en_peligro = barco_mas_daniado barco barco_adversario
    putStrLn ("Ambos contricantes recargan sus armas. El " ++ nombreBarco barco_en_peligro ++ " se ve severamente dañado.")
    putStrLn (relatos_de_la_batalla (valorAleatorio cantidad_de_relatos))
    batalla_barcos [] barco barco_adversario

batalla_barcos (head:body) barco barco_adversario = do
    putStrLn ("Los cañones del " ++ nombreBarco barco_adversario ++ " enemigo abren fuego tí y mientras tu y el " ++ nombreBarco barco ++ " responden sin piedad.")
    putStrLn (relatos_de_la_batalla (valorAleatorio cantidad_de_relatos))
    batalla_barcos body barco barco_adversario


barco_mas_daniado :: Barco -> Barco -> Barco
barco_mas_daniado barco barco_adversario
  | length(tripulacion barco) >= length (tripulacion barco_adversario) = barco_adversario
  | otherwise = barco


get_protagonista :: Barco -> Pirata
get_protagonista barco = head (tripulacion barco)

--- FUNCIONES AUXILIARES
mock_end :: IO String
mock_end = return "End"


tesoroAleatorio :: [Tesoro] -> IO Tesoro 
tesoroAleatorio tesoros = valorAleatorio tesoros

tesorosAleatorios :: [Tesoro] -> IO [Tesoro] 
tesorosAleatorios tesoros = valoresAleatorios tesoros

islaAleatoria :: IO Isla 
islaAleatoria =  valorAleatorio islas

barcoAleatorio :: IO Barco
barcoAleatorio = valorAleatorio barcos

tripulantesAleatorios :: [Pirata] -> IO [Pirata]
tripulantesAleatorios tripulacion = valoresAleatorios tripulacion

contrincanteAleatorio :: [Barco] -> IO Barco
contrincanteAleatorio contrincantes = valorAleatorio contrincantes

valorAleatorio :: [a] -> IO a
valorAleatorio list = do
    i <- getStdRandom (randomR (0, length list - 1)) 
    return $ list !! i

valoresAleatorios :: [a] -> IO [a]
valoresAleatorios list = do
    i <- getStdRandom (randomR (0, length list)) 
    return $ take i list 

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

procesar_eleccion_ciudad :: String -> Pirata -> IO String    
procesar_eleccion_ciudad eleccion protagonista
  | eleccion == "1" = saquear_ciudad protagonista port_royal
  | eleccion == "2" = saquear_ciudad protagonista new_providence
  | otherwise = elegir_ciudad_a_saquear protagonista

procesar_eleccion_ciudad_asedio :: String -> Barco -> IO String   
procesar_eleccion_ciudad_asedio eleccion barco 
  | eleccion == "1" = bombardear_ciudad barco port_royal
  | eleccion == "2" = bombardear_ciudad barco new_providence
  | otherwise = elegir_ciudad_a_asediar barco 

realizar_intercambio :: Pirata -> Tesoro -> [Tesoro] -> Pirata    
realizar_intercambio protagonista tesoroAEntregar = (entregar_tesoro tesoroAEntregar) . recibir_tesoros protagonista

elegir_tipo_saqueo :: String -> Pirata -> Ciudad -> IO String
elegir_tipo_saqueo eleccion protagonista ciudad
  | eleccion == "1" = sobornar_guardias protagonista ciudad
  | eleccion == "2" = combatir_guardias protagonista ciudad
  | otherwise = saquear_ciudad protagonista ciudad

recibir_tesoros :: Pirata -> [Tesoro] -> Pirata
recibir_tesoros pirata tesorosSaqueables  = pirata { botin = (botin pirata) ++ (unsafePerformIO (tesorosAleatorios tesorosSaqueables)) }

entregar_tesoro :: Tesoro -> Pirata -> Pirata
entregar_tesoro tesoroAEntregar pirata = pirata {botin = delete tesoroAEntregar (botin pirata)} 

ver_estado :: Pirata -> (a -> IO String) -> a -> IO String
ver_estado protagonista menu_opciones argumento = do
    putStrLn("\n")
    putStrLn(show protagonista)
    putStrLn("\n")
    menu_opciones argumento

cantidad_tesoros_valiosos :: Pirata -> Int 
cantidad_tesoros_valiosos pirata =
   length (filter (es_valioso) (botin pirata))

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
  | otherwise =  nombreEnMinusculas tesoro

--- NOMBRES DE TESOROS - CON ARTICULOS

una = [biciCopada, brujula, cajitaMusical, moneda, espada, media_sucia]
unas = [zapatillasViotti, zapatillasDini]
un = [frascoJack, frascoAnne, cuchillo]
unos = [doblones]

nombre_con_articulo :: Tesoro -> String
nombre_con_articulo tesoro
  | elem tesoro unos = "unos " ++ nombreEnMinusculas tesoro
  | elem tesoro unas  = "unas " ++ nombreEnMinusculas tesoro
  | elem tesoro una = "una " ++ nombreEnMinusculas tesoro
  | elem tesoro un = "un " ++ nombreEnMinusculas tesoro
  | otherwise = nombreEnMinusculas tesoro

nombreEnMinusculas :: Tesoro -> String
nombreEnMinusculas tesoro = lowerString (nombreTesoro tesoro)

lowerString :: String -> String
lowerString = map toLower
