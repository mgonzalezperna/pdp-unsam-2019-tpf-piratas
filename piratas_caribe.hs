import           Data.List
import qualified Data.Set            as Set
import           Text.Show.Functions

data Pirata = Pirata
  { nombrePirata :: String
  , botin        :: [Tesoro]
  } deriving (Show)

instance Eq Pirata where
  pirata1 == pirata2 = (nombrePirata pirata1) == (nombrePirata pirata2)

instance Ord Pirata where
  pirata1 <= pirata2 = (nombrePirata pirata1) <= (nombrePirata pirata2)

data Tesoro
  = Tesoro { nombreTesoro :: String
           , valor        :: Double }
  | Bono { cotizaciones :: [Cotizacion] }
  | Leliq { importe_nominal :: Double
            , pais_emisor :: Pais }
  deriving (Show, Eq)

nombre_tesoro :: Tesoro -> String
nombre_tesoro (Tesoro nombre _) = nombre
nombre_tesoro (Bono _) = "Bono"
nombre_tesoro (Leliq _ pais) = "Leliq " ++ (nombre_del_pais pais)

valor_tesoro :: Tesoro -> Double
valor_tesoro (Bono cotizaciones) = valor_bono cotizaciones
valor_tesoro (Leliq importe pais) = (tasa_segun_pais pais) * importe

data Barco = Barco
  { tripulacion     :: Tripulacion
  , nombreBarco     :: String
  , forma_saqueo    :: FormaSaqueo
  } deriving (Show)

instance Ord Barco where
  barco1 <= barco2 = (length (tripulacion barco1)) <= (length (tripulacion barco2))

instance Eq Barco where
  barco1 == barco2 =
    (compara_nombres barco1 barco2) &&
    (compara_cantidad_tripulantes barco1 barco2)

compara_nombres :: Barco -> Barco -> Bool
compara_nombres barco1 barco2 =
  Set.fromList (tripulacion barco1) ==
  Set.fromList (tripulacion barco2)

compara_cantidad_tripulantes :: Barco -> Barco -> Bool
compara_cantidad_tripulantes barco1 barco2 =
  length (tripulacion barco1) == length (tripulacion barco2)

data Isla = Isla
  { elemento_tipico :: Tesoro
  , nombreIsla      :: String
  } deriving (Show, Eq)

data Ciudad = Ciudad
  { tesoros_disponibles :: [Tesoro]
  , nombreCiudad        :: String
  } deriving (Show, Eq)

data Pais = Pais
  { nombre_del_pais :: String
  , tasa_segun_pais :: Double
  } deriving (Show, Eq)

type Tripulacion = [Pirata]

type FormaSaqueo = Tesoro -> Bool

type Cotizacion = Double

type Perfil = Barco -> Barco

type Situacion = Barco -> Barco


universidad_anti_dictaminante :: Barco -> Barco
universidad_anti_dictaminante barco = barco {forma_saqueo = not .(forma_saqueo barco)}


universidad_buitres_alternativos :: Barco -> Barco
universidad_buitres_alternativos barco =
  barco
    { forma_saqueo =
        forma_compleja
          [forma_saqueo barco, solo_tesoros_valiosos, saqueo_buitre]
    }

universidad_atlantica_inofensiva :: Barco -> Barco
universidad_atlantica_inofensiva = id

valor_bono :: [Cotizacion] -> Double
valor_bono = (1.5 *) . diferencia_cotizaciones

diferencia_cotizaciones :: [Cotizacion] -> Double
diferencia_cotizaciones list_cotizaciones =
  abs (minimum list_cotizaciones - maximum list_cotizaciones)

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
oro = Tesoro {nombreTesoro = "Oro", valor = 750}

ron :: Tesoro
ron = Tesoro {nombreTesoro = "Ron", valor = 25}

--Bonos
bono_cavallo :: Tesoro
bono_cavallo = Bono ([2000, 3000, 9000, 5000])

bono_u2 :: Tesoro
bono_u2 = Bono ([300, 900, 700])

--LeLiq
leliq_argentino :: Tesoro
leliq_argentino = Leliq { importe_nominal = 10000, pais_emisor = argentina}

leliq_brasilero :: Tesoro
leliq_brasilero = Leliq { importe_nominal = 400, pais_emisor = brasil}

leliq_mexicano :: Tesoro
leliq_mexicano = Leliq { importe_nominal = 9000, pais_emisor = mexico}


--PAISES
argentina :: Pais
argentina = Pais {nombre_del_pais = "Argentina", tasa_segun_pais = 1.74}

mexico :: Pais
mexico = Pais {nombre_del_pais = "Mexico", tasa_segun_pais = 1.34}

brasil :: Pais
brasil = Pais {nombre_del_pais = "Brasil", tasa_segun_pais = 1.12}

paises :: [Pais]
paises = [argentina, mexico, brasil]

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
davidJones =
  Pirata
    { nombrePirata = "David Jones"
    , botin = [biciCopada, zapatillasDini, cajitaMusical, cuchillo]
    }

anneBonny :: Pirata
anneBonny = Pirata {nombrePirata = "Anne Bonny", botin = [doblones, frascoAnne]}

elizabethSwann :: Pirata
elizabethSwann =
  Pirata {nombrePirata = "Elizabeth Swann", botin = [moneda, espada]}

--BARCOS
perla =
  Barco
    { tripulacion = [jackSparrow, anneBonny]
    , nombreBarco = "Perla Negra"
    , forma_saqueo = solo_tesoros_valiosos
    }

holandes =
  Barco
    { tripulacion = [davidJones]
    , nombreBarco = "Holandes Errante"
    , forma_saqueo = solo_tesoros_valiosos
    }

venganza_reina_ana =
  Barco
    { tripulacion = [viotti, dini]
    , nombreBarco = "Venganza de la Reina Ana"
    , forma_saqueo =
        forma_compleja [saqueo_fobico "oro", saqueo_buitre, solo_tesoros_valiosos]
    }

mary_celeste = 
  Barco
    { tripulacion = generar_tripulacion_infinita 
    , nombreBarco = "Mary Celeste"
    , forma_saqueo = solo_tesoros_valiosos
    }

--ISLAS
isla_tortuga = Isla {elemento_tipico = frascoAnne, nombreIsla = "Isla Tortuga"}

isla_ron = Isla {elemento_tipico = ron, nombreIsla = "Isla del Ron"}

--CIUDADES
port_royal =
  Ciudad
    { tesoros_disponibles = [oro, cuchillo, espada, biciCopada]
    , nombreCiudad = "Port Royal"
    }

carmen_de_patagones =
  Ciudad
    { tesoros_disponibles = [oro, bono_cavallo, leliq_brasilero]
    , nombreCiudad = "Carmen de Patagones"
    }

--TESOROS PIRATAS
cantidad_tesoros :: Pirata -> Int
cantidad_tesoros = length . botin

es_afortunado :: Pirata -> Bool
es_afortunado = (> 10000) . sum . valores_tesoros

valores_tesoros :: Pirata -> [Double]
valores_tesoros pirata = map valor (botin pirata)

comparar_nombres_tesoros :: Tesoro -> Tesoro -> Bool
comparar_nombres_tesoros tesoro_1 tesoro_2 =
  nombreTesoro tesoro_1 == nombreTesoro tesoro_2

valores_distintos_tesoros :: Tesoro -> Tesoro -> Bool
valores_distintos_tesoros tesoro_1 tesoro_2 = valor tesoro_1 /= valor tesoro_2

nombres_iguales_y_valores_distintos :: Tesoro -> Tesoro -> Bool
nombres_iguales_y_valores_distintos tesoro_1 tesoro_2 =
  comparar_nombres_tesoros tesoro_1 tesoro_2 &&
  valores_distintos_tesoros tesoro_1 tesoro_2

alguno_cumple_nombres_iguales_valores_distintos :: [Tesoro] -> Tesoro -> Bool
alguno_cumple_nombres_iguales_valores_distintos botin tesoro =
  any (nombres_iguales_y_valores_distintos tesoro) botin

tienen_mismo_tesoro_y_valor_diferente :: Pirata -> Pirata -> Bool
tienen_mismo_tesoro_y_valor_diferente pirata =
  any (alguno_cumple_nombres_iguales_valores_distintos (botin pirata)) . botin

valor_tesoro_mas_valioso :: Pirata -> Double
valor_tesoro_mas_valioso = maximum . valores_tesoros

adquirir_tesoro :: Pirata -> Tesoro -> Pirata
adquirir_tesoro pirata tesoro
 = pirata {botin = tesoro : botin pirata}

perder_tesoros_valiosos :: Pirata -> Pirata -- se puede delegar qué cosa es un tesoro valioso, y usarlo acá
perder_tesoros_valiosos pirata
 = pirata {botin = (filter ((< 100) . valor) (botin pirata))}

perder_tesoros_con_nombre :: String -> Pirata -> Pirata -- tesoros con nombre = tesoros específicos
perder_tesoros_con_nombre nombre pirata =
  pirata {botin = (filter ((/= nombre) . nombreTesoro) (botin pirata))}

--TEMPORADA DE SAQUEOS

saquear :: Pirata -> FormaSaqueo -> Tesoro -> Pirata
saquear pirata forma tesoro
  | forma tesoro = adquirir_tesoro pirata tesoro
  | otherwise = pirata

solo_tesoros_valiosos :: FormaSaqueo -- y reutilizar tesoro valioso acá.
solo_tesoros_valiosos = (> 100) . valor

solo_tesoros_especificos :: String -> FormaSaqueo -- tesoros con nombre = tesoros específicos
solo_tesoros_especificos clave = (== clave) . nombreTesoro

pirata_con_corazon :: FormaSaqueo
pirata_con_corazon tesoro = False

-- Saqueos sofisticados
--Buitres: Permite elegir cualquier tesoro que sea un bono en dafault.
saqueo_buitre :: FormaSaqueo
saqueo_buitre tesoro = "Bono" == (nombreTesoro tesoro)

--Permite tomar cualquier tesoro, excepto los que su nombre sea una palabra dada, que representa la cosa a la que se le tiene fobia
saqueo_fobico :: String -> FormaSaqueo
-- saqueo_fobico fobia tesoro = fobia /= (nombreTesoro tesoro)
saqueo_fobico fobia = not . (solo_tesoros_especificos fobia)

--Condicion de cumplimiento del any
evaluar :: Tesoro -> FormaSaqueo -> Bool
evaluar tesoro forma = forma tesoro

forma_compleja :: [FormaSaqueo] -> Tesoro -> Bool
forma_compleja formas tesoro = any (evaluar tesoro) formas

-- NAVEGANDO LOS SIETE MARES
incorporar_a_tripulacion :: Pirata -> Barco -> Barco
incorporar_a_tripulacion pirata barco =
  barco {tripulacion = (tripulacion barco) ++ [pirata]}

tripulacion_infinita :: Barco -> Barco
tripulacion_infinita barco =
  barco
    { tripulacion =
        map
          (generar_pirata_distinto
             Pirata {nombrePirata = "semilla", botin = [ron]})
          [1 ..]
    }
generar_tripulacion_infinita :: Tripulacion
generar_tripulacion_infinita = map (generar_pirata_distinto Pirata {nombrePirata = "Lucas", botin = [ron]}) [1 ..]

generar_pirata_distinto :: Pirata -> Double -> Pirata
generar_pirata_distinto pirata_modelo numero =
  pirata_modelo {nombrePirata = "Lucas " ++ show numero}

abandonar_tripulacion :: Pirata -> Barco -> Barco
abandonar_tripulacion pirata barco =
  barco {tripulacion = delete pirata (tripulacion barco)}

anclar_en_isla :: Barco -> Isla -> Barco
anclar_en_isla barco isla =
  barco {tripulacion = tomar_tesoros (tripulacion barco) (elemento_tipico isla)}

tomar_tesoros :: Tripulacion -> Tesoro -> Tripulacion
tomar_tesoros tripulacion tesoro = map (flip adquirir_tesoro tesoro) tripulacion

atacar_ciudad :: Barco -> Ciudad -> Barco
atacar_ciudad barco ciudad
  | mas_tesoros_que_tripulantes (tesoros_disponibles ciudad) (tripulacion barco) =
    saquear_ciudad barco ciudad
  | otherwise =
    saquear_ciudad
      (echar_piratas barco (length (tesoros_disponibles ciudad)))
      ciudad

--saquear_con_zipWith :: Barco -> Ciudad -> Barco
--saquear_con_zipWith barco ciudad = barco {tripulacion = zipWith (saquear (forma_saqueo barco)) (tripulacion barco) (tesoros_disponibles ciudad)}
saquear_ciudad :: Barco -> Ciudad -> Barco
saquear_ciudad barco ciudad =
  barco
    { tripulacion =
        (obtener_tesoros_por_tripulante
           (forma_saqueo barco)
           (tripulacion barco)
           (tesoros_disponibles ciudad))
    }

obtener_tesoros_por_tripulante ::
     FormaSaqueo -> Tripulacion -> [Tesoro] -> Tripulacion
obtener_tesoros_por_tripulante formaRobarGanador tripulacionGanador tesorosPerdedor =
  map
    (tomar_si_le_interesa (formaRobarGanador))
    (zip tripulacionGanador tesorosPerdedor)

tomar_si_le_interesa :: FormaSaqueo -> (Pirata, Tesoro) -> Pirata
tomar_si_le_interesa forma_saqueo (pirata, tesoro) =
  saquear pirata forma_saqueo tesoro

echar_piratas :: Barco -> Int -> Barco
echar_piratas barco quedan =
  barco {tripulacion = (take quedan (tripulacion barco))}

mas_tesoros_que_tripulantes :: [Tesoro] -> Tripulacion -> Bool
mas_tesoros_que_tripulantes tesoros tripulantes =
  (length tesoros) >= (length tripulantes)

abordar :: Barco -> Barco -> (Barco, Barco)
abordar barco1 barco2
  | length (tripulacion barco1) >= length (tripulacion barco2) =
    sacarle_todos_los_tesoros barco1 barco2
  | otherwise = sacarle_todos_los_tesoros barco2 barco1

sacarle_todos_los_tesoros :: Barco -> Barco -> (Barco, Barco)
sacarle_todos_los_tesoros ganador perdedor =
  ( ganador
      { tripulacion =
          (obtener_tesoros_por_tripulante
             (solo_tesoros_valiosos)
             (tripulacion ganador)
             (todos_los_tesoros (tripulacion perdedor)))
      }
  , perdedor
      {tripulacion = (map perder_tesoros_valiosos (tripulacion perdedor))})

todos_los_tesoros :: Tripulacion -> [Tesoro]
todos_los_tesoros tripulacion = concat (map botin tripulacion)

-- HACETE TODA LA PELÍCULA
escena1 :: Barco
escena1 = anclar_en_isla perla isla_ron

escena2 :: Barco
escena2 = anclar_en_isla holandes isla_tortuga

historia_perla_negra :: Barco
historia_perla_negra = atacar_ciudad escena1 port_royal

historia_holandes_errante :: Barco
historia_holandes_errante = atacar_ciudad escena2 carmen_de_patagones

pelicula :: (Barco, Barco)
pelicula = abordar historia_perla_negra historia_holandes_errante

super_pelicula ::
     Barco -> Barco -> Isla -> Isla -> Ciudad -> Ciudad -> (Barco, Barco)
super_pelicula barco barco2 isla isla2 ciudad ciudad2 =
  abordar
    (atacar_ciudad (anclar_en_isla barco isla) ciudad)
    (atacar_ciudad (anclar_en_isla barco2 isla2) ciudad2)

ingresar_a_laboratorio :: (Barco -> Barco) -> Barco -> Barco
ingresar_a_laboratorio universidad barco = universidad barco

-- Historias de Barcos
una_lista_de_situaciones =
  [ incorporar_a_tripulacion dini
  , (fst . (flip abordar holandes))
  , flip atacar_ciudad port_royal
  ]

aplicar_situacion :: Barco -> Situacion -> Barco
aplicar_situacion barco situacion = situacion barco

aplicar_situaciones :: [Situacion] -> Barco -> Barco
aplicar_situaciones situaciones barco =
  foldl aplicar_situacion barco situaciones

historia_inofensiva_para :: [Situacion] -> [Barco] -> [Barco]
historia_inofensiva_para situaciones barcos = filter (quedo_igual situaciones) barcos

quedo_igual :: [Situacion] -> Barco -> Bool
quedo_igual situaciones barco = barco == (aplicar_situaciones situaciones barco)  

barcos_despues_de_situaciones :: [Barco] -> [Situacion] -> [Barco]
barcos_despues_de_situaciones barcos situaciones = map (aplicar_situaciones situaciones) barcos

mas_tripulantes_despues_de_historia :: [Situacion] -> [Barco] -> Barco
mas_tripulantes_despues_de_historia situaciones barcos =  barco_mas_numeroso (barcos_despues_de_situaciones barcos situaciones)

barco_mas_numeroso :: [Barco] -> Barco
barco_mas_numeroso barcos = maximum barcos

-- anclar_en_isla => itera para siempre
-- incorporar_a_tripulacion => no alcanza a incorporar el pirata nuevo (la función lo agrega al final)
-- abandonar_tripulacion => funciona - abandonar_tripulacion ((tripulacion mary_celeste) !! 1) mary_celeste
-- atacar_ciudad => cuelga
-- echar_piratas => funciona OK
-- abordar => falla
-- sacarle_todos_los_tesoros => si el infinito es ganador, funciona. Si es el perdedor, itera para siempre
