{-# LANGUAGE BlockArguments #-}
module Main where

import Piratas_Caribe
import Auxiliares
import System.IO.Unsafe
import Data.List as List

--HISTORIA
main :: IO () 
main = do
    putStrLn("Ahoy novato! Estas aquí para convertirte en un poderoso pirata!")
    putStrLn("Es hora de comenzar tu aventura! Pero antes...")
    protagonista <- crear_pirata
    putStrLn("Ahora es hora de salir a navegar los 7 mares! Tu historia comienza en la isla Tortuga.")
    resultado_historia <- menu_historia protagonista
    putStrLn(resultado_historia)
    putStrLn("\n\nFin. Gracias por jugar!")
    putStrLn("\n\nHecho con <3 por Alejandro Dini, Nicolás Viotti y Martin Gonzalez Perna.\n")

crear_pirata :: IO (Pirata)
crear_pirata = do
    putStrLn("¿Cuál es tu nombre?\n")
    nombre_protagonista <- getLine
    -- Depurar input de nombre de Pirata.
    putStrLn("Bienvenido " ++ nombre_protagonista ++ "!")
    putStrLn("Ahora que ya tienes un nombre, te lanzas a la aventura! Prepara tu botín y...")
    suspenso(1)
    putStrLn("... no tienes un botín, verdad?")
    suspenso(2)
    putStrLn("... bueno, en este caso, tu botín inicial consiste en... una media sucia!")
    putStrLn("Tomala!")
    suspenso(2)
    putStrLn("Si te quedas sin tesoros valiosos, serás expulsado de la vida pirata!")
    putStrLn("Los tesoros valiosos valen más de 100")
    putStrLn("Y recuerda que el valor sumado de tus tesoros determina tu fortaleza en combate.")
    putStrLn("Pero la cantidad de tesoros que lleves va a hacerte más lento y tendrás más chances de perder los combates!")
    putStrLn("\n")
    return Pirata {nombre_pirata = nombre_protagonista, botin = [media_sucia]}

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
    putStrLn("(4)-RECLUTAR TRIPULANTES")
    putStrLn("(5)-RETIRARSE DE LA PIRATERIA")
    putStrLn("(6)-VER MI ESTADO")
    putStrLn("(7)-VER ESTADO TRIPULACION")
    opcion <- getLine
    desarrollar_historia_en_barco opcion barco

desarrollar_historia :: String -> Pirata -> IO String 
desarrollar_historia opcion protagonista = case opcion of
     "1" -> robar_barco protagonista
     "2" -> elegir_ciudad_a_saquear protagonista
     "3" -> retirarse_de_la_pirateria protagonista
     "4" ->ver_estado protagonista menu_historia protagonista
     _ -> menu_historia protagonista

desarrollar_historia_en_barco :: String -> Barco -> IO String
desarrollar_historia_en_barco opcion barco = case opcion of
      "1" -> encuentro_barco barco 
      "2" -> anclar_en_isla_cercana barco 
      "3" -> elegir_ciudad_a_asediar barco
      "4" -> intentar_buscar_tripulacion barco
      "5" -> retirarse_de_la_pirateria (get_protagonista barco)
      "6" -> ver_estado (get_protagonista barco) menu_historia_con_barco barco
      "7" -> ver_estado_tripulacion (barco) menu_historia_con_barco barco
      _ -> menu_historia_con_barco barco


-- ACCIONES POSIBLES

robar_barco :: Pirata -> IO String
robar_barco protagonista = do
    putStrLn("Sólo hay un velero triste anclado en el muelle...")
    putStrLn("Pero... que va, lo tomas y zarpas. Enhorabuena! Ahora es tuyo. ¿Qué nombre le pondrás?\n")
    nombreNuevoBarco <- getLine
    -- Depurar input de nombre de Barco.
    menu_historia_con_barco Barco { tripulacion = [protagonista] , nombre_barco = nombreNuevoBarco}

--SAQUEO CIUDAD

elegir_ciudad_a_saquear :: Pirata -> IO String
elegir_ciudad_a_saquear protagonista = do
    putStrLn("\n¿Qué ciudad deseas saquear?")
    --Las opciones deberían mapearse del List de ciudades.
    putStrLn("(1)-Port Royal")
    putStrLn("(2)-New Providence")
    eleccion <- getLine
    procesar_eleccion_ciudad eleccion protagonista

saquear_ciudad :: Pirata -> Ciudad -> IO String
saquear_ciudad protagonista ciudad = do
    putStrLn(concat ["Te dirijes con paso firme a la ciudad de " , (nombre_ciudad ciudad) , " dispuesto a llevarte todos sus tesoros!!"])
    putStrLn("Pero en la entrada te detienen unos guardias con cara de pocos amigos. Te piden que te retires... ¿Qué decisión tomas?\n")
    putStrLn("(1)-SOBORNAR A LOS GUARDIAS PARA QUE TE DEN ALGUNOS TESOROS")
    putStrLn("(2)-COMBATIR CONTRA LOS GUARDIAS\n")
    eleccion <- getLine
    elegir_tipo_saqueo eleccion protagonista ciudad

sobornar_guardias :: Pirata -> Ciudad -> IO String
sobornar_guardias protagonista ciudad
  | cantidad_tesoros_valiosos protagonista > 0 = soborno_exitoso protagonista ciudad
  | otherwise = encarcelamiento protagonista
    
soborno_exitoso :: Pirata -> Ciudad -> IO String
soborno_exitoso protagonista ciudad = do
    let tesoro_a_entregar = unsafePerformIO (tesoro_aleatorio (botin protagonista))
    putStrLn("Ofreces uno de tus tesoros a los guardias. \nEllos eligen " ++ nombre_con_articulo tesoro_a_entregar)
    putStrLn("Lo entregas y te dan acceso a la boveda de los tesoros a cambio\n")
    menu_historia (realizar_intercambio protagonista tesoro_a_entregar (tesoros_saqueables ciudad))

combatir_guardias :: Pirata -> Ciudad -> IO String
combatir_guardias protagonista ciudad = do
    putStrLn("Ningun guardia en todo el caribe puede ser capaz de extorsionarte sin llevarse su merecido!")
    resultado_combate_guardias protagonista ciudad

resultado_combate_guardias :: Pirata -> Ciudad -> IO String
resultado_combate_guardias protagonista ciudad 
  | unsafePerformIO (fuerza_golpe_protagonista protagonista) > unsafePerformIO (fuerza_golpe_contrincante protagonista) = ganar_combate protagonista ciudad
  | otherwise = perder_combate protagonista ciudad

ganar_combate :: Pirata -> Ciudad -> IO String
ganar_combate protagonista ciudad = do
    putStrLn("Con un golpe certero del sable desarmas a tus contrincantes, que huyen atemorizados ante tu habilidad. El tesoro queda a tu merced")
    menu_historia (adquirir_tesoro_aleatorio protagonista (tesoros_saqueables ciudad))

perder_combate :: Pirata -> Ciudad -> IO String
perder_combate protagonista ciudad = do
  putStrLn("Con ese golpe certero te desarma y quedas a su merced. Tus enemigos eran más habiles de lo que esperabas.")
  putStrLn("No tienes más alternativa que entregar algunos de tus tesoros valiosos a cambio de que te dejen escapar.")
  putStrLn("Les lanzas algunos, manoteando en tu botín, y luego te lanzas a la carrera buscando la salida.")
  evaluar_si_continua_segun_tesoros (quedar_con_algunos_tesoros protagonista) encarcelamiento

encarcelamiento :: Pirata -> IO String
encarcelamiento protagonista = return "Pero los guardias saben no tienes ni un doblon de oro y van directo a tí. Te atrapan y no vacilan en absoluto en encerrarte en las mazmorras. Tus días de pirata están acabados.r"

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
realizar_intercambio protagonista tesoro_a_entregar = (entregar_tesoro tesoro_a_entregar) . adquirir_tesoro_aleatorio protagonista

elegir_tipo_saqueo :: String -> Pirata -> Ciudad -> IO String
elegir_tipo_saqueo eleccion protagonista ciudad
  | eleccion == "1" = sobornar_guardias protagonista ciudad
  | eleccion == "2" = combatir_guardias protagonista ciudad
  | otherwise = saquear_ciudad protagonista ciudad

ver_estado :: Pirata -> (a -> IO String) -> a -> IO String
ver_estado protagonista menu_opciones argumento = do
    putStrLn("\n")
    putStrLn(show protagonista)
    menu_opciones argumento

ver_estado_tripulacion :: Barco -> (a -> IO String) -> a -> IO String
ver_estado_tripulacion barco menu_opciones argumento = do
    putStrLn("\n")
    estado_piratas (tripulacion barco)
    menu_opciones argumento


--DESDE BARCO

anclar_en_isla_cercana :: Barco -> IO String
anclar_en_isla_cercana barco = do
    isla <- isla_aleatoria
    putStrLn("Los vientos de los siete mares te arrastran hacia la isla más cercana.")
    putStrLn("En el horizonte se vislumbra el contorno de la isla " ++ (nombre_isla isla))
    putStrLn("Cuando desenbarcan, ven un enorme depósito de " ++ nombre_plural(elemento_tipico isla) ++ ".")
    putStrLn("Añades el tesoro a tu botín pero cuando vuelves a subir a tu nave, te sientes más liviano... Es posible que se te haya caído algún tesoro en la visita?")
    let protagonista = perder_tesoros_aleatorios (get_protagonista barco)
    let nuevo_barco = barco_con_protagonista_modificado protagonista barco
    menu_historia_con_barco $ anclar_en_isla nuevo_barco isla

elegir_ciudad_a_asediar :: Barco -> IO String
elegir_ciudad_a_asediar barco = do
    putStrLn("\n¿Qué ciudad deseas asediar?")
    --Las opciones deberían mapearse del List de ciudades.
    putStrLn("(1)-Port Royal") 
    putStrLn("(2)-New Providence")
    eleccion <- getLine
    procesar_eleccion_ciudad_asedio eleccion barco

bombardear_ciudad :: Barco -> Ciudad -> IO String
bombardear_ciudad barco ciudad= do
    putStrLn("El barco se alinea, mientras se preparan los cañones y apuntan al fuerte de la ciudad. Te preparas a dar la voz de mando...")
    putStrLn("PREPAREN")
    putStrLn("APUNTEN")
    putStrLn("FUEGOO!")
    suspenso(2)
    (resultado_asedio (valor_aleatorio [0,1])) barco ciudad

resultado_asedio :: IO Int -> (Barco -> Ciudad -> IO String)
resultado_asedio resultado_asedio
  | (unsafePerformIO resultado_asedio) == 1 = invadir_ciudad
  | otherwise = ser_repelidos_por_ciudad

invadir_ciudad :: Barco -> Ciudad -> IO String
invadir_ciudad barco ciudad = do
    putStrLn("Las murallas estallan en pedazos y rapidamente te escabulles dentro del fuerte de la ciudad buscando un botín.")
    botin_adquirido <- tesoro_aleatorio( tesoros_saqueables ciudad )
    putStrLn("Los guardias te persiguen por lo que solo eres capaz de tomar " ++ nombre_con_articulo botin_adquirido ++ " antes de volver a embarcar.")
    let protagonista = get_protagonista barco
    let protagonista_con_tesoro = protagonista { botin = botin (protagonista) ++ [botin_adquirido] }
    menu_historia_con_barco (barco { tripulacion = protagonista_con_tesoro:(List.drop 1 (tripulacion barco))})

ser_repelidos_por_ciudad :: Barco -> Ciudad -> IO String
ser_repelidos_por_ciudad barco ciudad = do
    putStrLn("... pero nada ha pasado")
    putStrLn("Quizás hubiese sido buena idea revisar el estado de la polvora antes de atacar...")
    suspenso(1)
    putStrLn("Intentas una maniobra evasiva pero... Demasiado tarde! Los cañones del fuerte arrasan con tu nave y de pronto te encuentras escupiendo agua y arena en la playa.")
    putStrLn("Mientras te repones y revisas si el mar te arrancó algunos tesoros, visualizas muy cerca la entrada a la ciudad. Los guardias parecen distraídos mirando tu corsario hundirse sin remedio.")
    evaluar_si_continua_segun_tesoros (quedar_con_algunos_tesoros(get_protagonista barco)) sin_tesoro_valioso

--- RECLUTAR TRIPULANTES

intentar_buscar_tripulacion :: Barco -> IO String
intentar_buscar_tripulacion barco
  | length (tripulacion barco) >= length piratas = imposible_reclutar_mas_piratas barco
  | cantidad_tesoros_valiosos (get_protagonista barco) > 1 = buscar_posible_tripulacion barco
  | otherwise = expulsado_de_taberna barco

imposible_reclutar_mas_piratas :: Barco -> IO String
imposible_reclutar_mas_piratas barco = do
    putStrLn("El bar parece completamente vacío, como si no quedaran más piratas que reclutar...")
    menu_historia_con_barco barco

expulsado_de_taberna :: Barco -> IO String
expulsado_de_taberna barco = do
    putStrLn("El pirata que cuida la entrada a la cantina te echa una severa mirada durante unos segundos y de pronto...")
    suspenso(1)
    putStrLn("...estalla en carcajadas.")
    suspenso(1)
    putStrLn("- No hay lugar en éste sitio para un pirata sin al menos DOS tesoros que tenga algún valor... Vete de aquí y vuelve cuando seas algo más que un pobre diablo!- te contesta al reponerse.")
    putStrLn("Parece que ningún ruin va a querer unirse a la tripulación del " ++ nombre_barco barco ++ " sin recibir al menos una ofrenda valiosa de tu parte.")
    menu_historia_con_barco barco

buscar_posible_tripulacion :: Barco -> IO String
buscar_posible_tripulacion barco = do
    putStrLn("Divisas la taberna más cercana y mal oliente de la región y pones rumbo a ella.")
    putStrLn("Al entrar, te acercas a la barra y pides un jarro de Grog. Una figura cerca tuyo te mira fijo.")
    putStrLn("Friamente, devuelves la mirada mientras sorbes tu trago...")
    suspenso(1)
    putStrLn("El Grog es realmente asqueroso.")
    suspenso(1)
    putStrLn("Mientras contienes las lágrimas de asco y picor, intentas reafirmarte poniendo tu cara más ruda y gritas roncamente:")
    putStrLn("-QUIEN DE TODOS USTEDES QUIERE DEMOSTRAR EL FEROZ PIRATA QUE PUEDE SER?")
    putStrLn("Una voz desde el fondo contesta")
    putStrLn("-LO HARÉ... A CAMBIO DE TU TESORO MÁS VALIOSO!")
    putStrLn("Mientras maldices su avaricia, buscas en tu inventario por el primer tesoro que puedas hallar...")
    suspenso(1)
    let tesoro_a_ofrecer = tesoro_mas_valioso (botin (get_protagonista barco))
    putStrLn("-OFREZCO " ++ upperString (nombre_tesoro tesoro_a_ofrecer) ++ "!")
    suspenso(1)
    --TODO: Acá podríamos incorporar una evaluación si el nuevo tripulante acepta o no dependiendo del valor del tesoro.
    putStrLn("Tu interlocutor se encoje de hombros y acepta. -De acuerdo... Pero huyamos rápido antes de que se den cuenta que no pagué mi bebida!")
    let nuevo_tripulante = unsafePerformIO (obtener_nuevo_tripulante (barco))
    putStrLn("Tu nuevo tripulante y tú se escabullen a la salida sigilosamente sin ningún remordimiento. Felicidades! " ++ nombre_pirata nuevo_tripulante ++ " se suma al " ++ nombre_barco barco ++ "!")
    menu_historia_con_barco (barco 
        {tripulacion = entregar_tesoro tesoro_a_ofrecer (get_protagonista barco)
        : (tail (tripulacion barco) ++ [nuevo_tripulante])})

--- BATALLA MARINA

encuentro_barco :: Barco -> IO String
encuentro_barco barco = do
    barco_adversario <- barco_aleatorio
    putStrLn("Desde el largavistas alcanzas a vislumbrar la silueta de un barco vulnerable.")
    putStrLn("Es el " ++ nombre_barco barco_adversario ++ "!")
    suspenso(1)
    putStrLn("Te lanzas a su encuentro a toda velocidad, preparando los cañones, listo para abordarlos.")
    batalla_barcos cantidad_de_turnos barco barco_adversario

cantidad_de_turnos :: [Integer]
--list que define la cantidad tope de turnos de la batalla, en este caso, 2.
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
    let en_ventaja = barco_en_ventaja barco barco_adversario 
    putStrLn ("Ya no quedan balas! Los tripulantes del " ++ nombre_barco en_ventaja ++ " lanzan tablones que caen pesadamente sobre su contrincante y se lanzan al abordaje. Ha llegado el momento de desfundar las espadas...")
    suspenso(1)
    pelea_abordaje barco barco_adversario

batalla_barcos [1] barco barco_adversario = do
    let en_ventaja = barco_en_ventaja barco barco_adversario 
    putStrLn ("Los barcos se alinean de cara a lanzarse a las armas una última vez. Los tripulantes del " ++ nombre_barco en_ventaja ++ " se ven confiados mientras se preparan para el abordaje.")
    let barco_post_batalla = perder_tripulantes_protagonista barco
    let barco_adversario_post_batalla = perder_tripulantes barco_adversario
    reporte_post_turno (tripulacion barco) (tripulacion barco_post_batalla)
    suspenso(4)
    putStrLn ("Ruegas que el barco resista un esfuerzo más...")
    suspenso(1)
    putStrLn (relatos_de_la_batalla (valor_aleatorio cantidad_de_relatos))
    turno [] barco_post_batalla barco_adversario_post_batalla 

batalla_barcos [1,2] barco barco_adversario = do
    putStrLn ("Los cañones del " ++ nombre_barco barco_adversario ++ " enemigo abren fuego tí y mientras tú desde el " ++ nombre_barco barco ++ " responden sin piedad.")
    let barco_post_batalla = perder_tripulantes_protagonista barco
    let barco_adversario_post_batalla = perder_tripulantes barco_adversario
    reporte_post_turno (tripulacion barco) (tripulacion barco_post_batalla)
    suspenso(4)
    putStrLn (relatos_de_la_batalla (valor_aleatorio cantidad_de_relatos))
    turno [1] barco_post_batalla barco_adversario_post_batalla 

reporte_post_turno :: [Pirata] -> [Pirata] -> IO() 
reporte_post_turno tripulacion_anterior tripulacion_resultante 
  | length (tripulacion_anterior) > length (tripulacion_resultante) = do
      putStrLn("Has perdido tripulantes!! Quedan " ++ show(length(tripulacion_resultante)-1) ++ "!!. Los piratas que cayeron al agua son:\n")
      estado_piratas ( obtener_grupo_sin_subgrupo tripulacion_anterior tripulacion_resultante )
  | otherwise = do
      putStrLn("La tripulacion sigue intacta!\n")
      estado_piratas ( tripulacion_anterior )

barco_en_ventaja :: Barco -> Barco -> Barco
barco_en_ventaja barco barco_adversario = head (List.delete (barco_mas_daniado barco barco_adversario) [barco, barco_adversario])

barco_mas_daniado :: Barco -> Barco -> Barco
barco_mas_daniado barco barco_adversario
  | length(tripulacion barco) >= length (tripulacion barco_adversario) = barco_adversario
  | otherwise = barco

turno :: [Integer] -> Barco -> Barco -> IO(String)
turno turnos barco barco_adversario
  | length (tripulacion barco) <= 1 && length (tripulacion barco_adversario) > 0 = perder_batalla barco
  | length (tripulacion barco) >= 1 && length (tripulacion barco_adversario) == 0 = ganar_batalla barco barco_adversario
  | otherwise = batalla_barcos turnos barco barco_adversario

perder_batalla :: Barco -> IO(String)
perder_batalla barco = do
  putStrLn("El " ++ nombre_barco barco ++ " empieza a escorar. Tu embarcación se va a pique!")
  putStrLn("Una ola te golpea con fuerza y caes por la borda. Alcanzas abrazas una tabla que te mantiene a flote.")
  putStrLn("Te desmayas y cuando despiertas, estás escupiendo arena en una playa que te resulta extrañamente conocida.")
  putStrLn("Revisas tu botín y apenas un par de tesoros sobrevivieron tu aventura a la deriva.")
  evaluar_si_continua_segun_tesoros (quedar_con_algunos_tesoros(get_protagonista barco)) sin_tesoro_valioso

ganar_batalla :: Barco -> Barco -> IO(String)
ganar_batalla barco barco_adversario
  | unsafePerformIO(sucede_evento_sobrenatural) = invocar_a_calypso barco
  | otherwise = do
      putStrLn("El " ++ nombre_barco barco_adversario ++ " se va a pique! Alcanzas a ver como la bodega de tesoros se rompe e inunda")
      putStrLn("Los tesoros se van a ir directo a lecho marino!")
      putStrLn("Sin perder tiempo, te lanzas a un bote y remando con violencia, alcanzas a rescatar unos pocos tesoros de tu maltrecho contrincante.")
      menu_historia_con_barco (
        barco {
            tripulacion =  adquirir_tesoro_mas_valioso (get_protagonista barco) (tesoros_tripulantes barco_adversario) : (tail (tripulacion barco))})

sucede_evento_sobrenatural :: IO(Bool)  
sucede_evento_sobrenatural = do
  random_int <- valor_aleatorio [0..20]
  return $ random_int == 1

invocar_a_calypso :: Barco -> IO(String)
invocar_a_calypso barco = do
  putStrLn("El barco adversario comienza a volcarse hacia un lado! Parece que los han hundido")
  putStrLn("Sin embargo, también notas que el cielo empieza a cerrarse. Nubes negras se materializan en segundos...")
  putStrLn("El agua empieza a arremolinarse bajo ambos barcos, y del centro del torbellino una figura se materializa.")
  putStrLn("\n CALIPSO \n")
  suspenso(2)
  putStrLn("Con una violencia inusitada, Calipso destroza los restos de ambos barcos, pero cuando estás a punto de caer al olvido, te materializas de nuevo en la costa.")
  putStrLn("De tu tripulación no quedan ni rastros. Pero al menos aún puedes contar con seguir con vida.")
  evaluar_si_continua_segun_tesoros (quedar_con_algunos_tesoros(get_protagonista barco)) sin_tesoro_valioso
  
pelea_abordaje :: Barco -> Barco -> IO(String)
pelea_abordaje barco barco_adversario = do
  rival <- tripulante_aleatorio (tripulacion barco_adversario)
  relato_pelea_abordaje rival
  resultado_mano_a_mano barco barco_adversario rival

relato_pelea_abordaje :: Pirata -> IO ()
relato_pelea_abordaje rival = do
  putStrLn("Las espadas chocan por doquier mientras los piratas combaten desesperadamente. Gritos y aullidos desgarran el aire mientras divisas a tu rival.")
  putStrLn("Es nada mas y nada menos que " ++ nombre_pirata rival ++ "!")
  putStrLn("Su mirada inyectada en furia se clava en tus ojos, y de pronto se lanza a tu encuentro.")

resultado_mano_a_mano :: Barco -> Barco -> Pirata -> IO(String)
resultado_mano_a_mano barco barco_adversario pirata_enfrentado
  | length (botin pirata_enfrentado) == 0 = ganar_combate_abordaje barco barco_adversario 
  | length (botin (get_protagonista barco)) == 0 = perder_combate_abordaje barco
  | otherwise = definicion_mano_a_mano barco barco_adversario pirata_enfrentado

definicion_mano_a_mano :: Barco -> Barco -> Pirata -> IO(String)
definicion_mano_a_mano barco barco_adversario pirata_enfrentado
  | unsafePerformIO (fuerza_golpe_protagonista (get_protagonista barco)) >= unsafePerformIO (fuerza_golpe_contrincante (pirata_enfrentado)) = ganar_combate_abordaje barco barco_adversario
  | otherwise = perder_combate_abordaje barco

ganar_combate_abordaje :: Barco -> Barco -> IO(String)  
ganar_combate_abordaje barco barco_adversario = do 
  putStrLn("Tu rival cae desarmado ante la potencia de tu golpe y toda su tripulación se rinde.")
  putStrLn("Decides perdonar la vida de todos los sobrevivientes a cambio de que se unan a tu cruzada.")
  putStrLn("Tu oferta se ve más atractiva que caminar por el tablón hacia los tiburones...")
  putStrLn("\nFelicitaciones! Has sumado nuevos tripulantes:\n")
  let nuevo_barco = incorporar_tripulantes barco (tripulacion barco_adversario)
  estado_piratas (tripulacion nuevo_barco)
  menu_historia_con_barco (nuevo_barco)

perder_combate_abordaje :: Barco -> IO(String)  
perder_combate_abordaje barco = do 
  putStrLn("Tus rival te ve caer al suelo con desprecio en sus ojos.")
  putStrLn("Patea tu espada lejos de tu alcance. Desde el suelo ves cómo uno a uno tus tripulantes son derrotados.")
  putStrLn("El abordaje fracasa, y luego de sacarte todos los tesoros valiosos, deciden lanzarte por la borda la mañana siguiente.")
  putStrLn("Sin embargo, uno de tus ex tripulantes por la noche abre la celda y te devuelve algunos de tus tesoros.")
  putStrLn("Sigilosamente, cada uno roba un bote y escapan con destinos distintos.")
  putStrLn("Vagas por días a la deriva, hasta que alcanzas ver tierra a la distancia!")
  putStrLn("Pero tu emoción se desvanece cuando comprendes que has vuelto al principio...")
  evaluar_si_continua_segun_tesoros (quedar_con_algunos_tesoros(get_protagonista barco)) sin_tesoro_valioso

evaluar_si_continua_segun_tesoros :: Pirata -> (Pirata -> IO String) -> IO String
evaluar_si_continua_segun_tesoros protagonista funcion_fin_del_juego
  | cantidad_tesoros_valiosos protagonista > 0 = menu_historia protagonista
  | otherwise = funcion_fin_del_juego protagonista

sin_tesoro_valioso :: Pirata -> IO(String)
sin_tesoro_valioso pirata = return "Sin embargo, al revisar tu botín, te das cuenta de que estás acabado. Sin un tesoro valioso, ya no te queda forma de continuar con tu aventura. Quizás sea hora de que dejes de jugar y que sigas haciendo la tarea de Paradigmas..."

retirarse_de_la_pirateria :: Pirata -> IO(String)
retirarse_de_la_pirateria pirata = do
    putStrLn("Miras al horizonte y decides que quizás ya fue suficiente.")
    putStrLn("Toda aventura tiene que tener un fin.")
    putStrLn("Y hoy, " ++ nombre_pirata pirata ++ " ha decidido poner fin a la suya.")
    return("Sin embargo, sabes que pasarán los años y cuando alguien pregunte por la historia de aquel jóven pirata que navegó por el Caribe, la gente responderá... \n\n" ++ rankear_pirata pirata)

rankear_pirata :: Pirata -> String 
rankear_pirata pirata
  | cantidad_tesoros_valiosos pirata < 2 = "¿¿Quién??" 
  | cantidad_tesoros_valiosos pirata < 3 = "Ah, si " ++ nombre_pirata pirata ++ ", el Cobarde..."
  | cantidad_tesoros_valiosos pirata < 4 = "Se, " ++ nombre_pirata pirata ++ ", el Bueno para nada."
  | cantidad_tesoros_valiosos pirata < 5 = "Oh, " ++ nombre_pirata pirata ++ ", el Gran pirata... que pudo ser más!"
  | otherwise = nombre_pirata pirata ++ ", la LEYENDA!" 

