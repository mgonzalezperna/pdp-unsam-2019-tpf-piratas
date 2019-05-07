import           Data.List
import qualified Data.Set            as Set
import           Text.Show.Functions

data Pirata = Pirata
  { nombrePirata :: String
  , botin        :: [Tesoro]
  } deriving (Show)

instance Eq Pirata where
  pirata1 == pirata2 = (nombrePirata pirata1) == (nombrePirata pirata2)

data Tesoro
  = Tesoro { nombreTesoro :: String
           , valor        :: Double }
  | Bono { nombreTesoro :: String
         , valor        :: Double
         , cotizaciones :: [Cotizacion] }
  deriving (Show, Eq)

data Barco = Barco
  { tripulacion     :: Tripulacion
  , nombreBarco     :: String
  , forma_saqueo    :: FormaSaqueo
  } deriving (Show)

instance Eq Barco where
  barco1 == barco2 =
    (compara_nombres barco1 barco2) &&
    (compara_cantidad_tripulantes barco1 barco2)

compara_nombres :: Barco -> Barco -> Bool
compara_nombres barco1 barco2 =
  Set.fromList (map nombrePirata (tripulacion barco1)) ==
  Set.fromList (map nombrePirata (tripulacion barco2))

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
  }

data Universidad = Universidad
  { perfil_academico :: Perfil
  }

type Tripulacion = [Pirata]

type FormaSaqueo = Tesoro -> Bool

type Cotizacion = Double

type Perfil = Barco -> Barco

type Situacion = Barco -> Barco


-- Universidades
universidad_anti_dictaminante :: Universidad
universidad_anti_dictaminante =
  Universidad {perfil_academico = perfil_anti_dictaminante}

perfil_anti_dictaminante :: Barco -> Barco
perfil_anti_dictaminante barco = barco {forma_saqueo = not .(forma_saqueo barco)}

universidad_buitres_alternativos :: Universidad
universidad_buitres_alternativos =
  Universidad {perfil_academico = perfil_buitre_alternativo}

perfil_buitre_alternativo :: Barco -> Barco
perfil_buitre_alternativo barco =
  barco
    { forma_saqueo =
        forma_compleja
          [forma_saqueo barco, solo_tesoros_valiosos, saqueo_buitre]
    }

universidad_atlantica_inofensiva :: Universidad
universidad_atlantica_inofensiva =
  Universidad {perfil_academico = perfil_inofensivo}

perfil_inofensivo :: Barco -> Barco
perfil_inofensivo barco = barco

--TESORO Bonos en dafault
bonos_en_dafault :: [Cotizacion] -> Tesoro
bonos_en_dafault list_cotizaciones =
  Bono
    { nombreTesoro = "Bono"
    , valor = valor_bono list_cotizaciones
    , cotizaciones = list_cotizaciones
    }

valor_bono :: [Cotizacion] -> Double
valor_bono = (1.5 *) . diferencia_cotizaciones

diferencia_cotizaciones :: [Cotizacion] -> Double
diferencia_cotizaciones list_cotizaciones =
  abs (minimum list_cotizaciones - maximum list_cotizaciones)

--TESORO Letras de liquidez
letras_de_liquidez :: Double -> String -> Tesoro
letras_de_liquidez valor_nominal nombre_pais =
  Tesoro
    { nombreTesoro = "LeLiq " ++ nombre_pais
    , valor = valor_letras valor_nominal nombre_pais
    }

valor_letras :: Double -> String -> Double
valor_letras valor_nominal nombre_pais =
  (tasa_del_pais nombre_pais) * valor_nominal

tasa_del_pais :: String -> Double
tasa_del_pais nombre_pais
  | existe_pais nombre_pais = tasa_segun_pais (buscar_pais nombre_pais)
  | otherwise = 0

existe_pais :: String -> Bool
existe_pais nombre_pais = elem nombre_pais (map (nombre_del_pais) paises)

buscar_pais :: String -> Pais
buscar_pais nombre_pais = head (filter (coinciden_nombres nombre_pais) paises)

coinciden_nombres :: String -> Pais -> Bool
coinciden_nombres nombre pais = nombre == (nombre_del_pais pais)

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
bono_cavallo = bonos_en_dafault [2000, 3000, 9000, 5000]

bono_u2 :: Tesoro
bono_u2 = bonos_en_dafault [300, 900, 700]

--LeLiq
leliq_argentino :: Tesoro
leliq_argentino = letras_de_liquidez 10000 "Argentina"

leliq_brasilero :: Tesoro
leliq_brasilero = letras_de_liquidez 400 "Brasil"

leliq_jamaiquino :: Tesoro
leliq_jamaiquino = letras_de_liquidez 9000 "Jamaica"

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
  -- Pirata (nombrePirata pirata) (tesoro : (botin pirata))
 = pirata {botin = tesoro : botin pirata}

perder_tesoros_valiosos :: Pirata -> Pirata -- se puede delegar qué cosa es un tesoro valioso, y usarlo acá
perder_tesoros_valiosos pirata
  -- Pirata (nombrePirata pirata) (filter ((< 100) . valor) (botin pirata))
 = pirata {botin = (filter ((< 100) . valor) (botin pirata))}

perder_tesoros_con_nombre :: String -> Pirata -> Pirata -- tesoros con nombre = tesoros específicos
perder_tesoros_con_nombre nombre pirata =
  pirata {botin = (filter ((/= nombre) . nombreTesoro) (botin pirata))}
  -- Pirata
  --   (nombrePirata pirata)
  --   (filter ((/= nombre) . nombreTesoro) (botin pirata))

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
  -- Barco
  --   ((tripulacion barco) ++ [pirata])
  --   (nombreBarco barco)
  --   (forma_saqueo barco)

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
  -- Barco
  --   (delete pirata (tripulacion barco))
  --   (nombreBarco barco)
  --   (forma_saqueo barco)

anclar_en_isla :: Barco -> Isla -> Barco
anclar_en_isla barco isla =
  barco {tripulacion = tomar_tesoros (tripulacion barco) (elemento_tipico isla)}
  -- Barco
  --   (tomar_tesoros (tripulacion barco) (elemento_tipico isla))
  --   (nombreBarco barco)
  --   (forma_saqueo barco)

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
  -- Barco
  --   (obtener_tesoros_por_tripulante (forma_saqueo barco) (tripulacion barco) (tesoros_disponibles ciudad) )
  --   (nombreBarco barco)
  --   (forma_saqueo barco)

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
  -- Barco
  --   (take quedan (tripulacion barco))
  --   (nombreBarco barco)
  --   (forma_saqueo barco)

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
  -- ( Barco
  --     (obtener_tesoros_por_tripulante (solo_tesoros_valiosos) (tripulacion ganador) (todos_los_tesoros (tripulacion perdedor)) )
  --     (nombreBarco ganador)
  --     (forma_saqueo ganador)
  -- , Barco
  --     (map perder_tesoros_valiosos (tripulacion perdedor))
  --     (nombreBarco perdedor)
  --     (forma_saqueo perdedor))

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
    -- Existen universidades cuya misión es desarrollar las habilidades de saqueo piratas. Cuando un barco va al laboratorio de una universidad, su forma de saqueo se ve alterada de acuerdo al perfil académico de dicha institución. Se tiene conocimiento de los siguientes perfiels, pero podría haber más:
    -- Universidad Anti Dictaminante de Estilos: Provoca que el barco tenga la forma de saque inversa a la que tenía. Por ejemplo si era "de corazón", ahora saquea todo; si era de objeto específico, se vuelve fóbica de dicho objeto y así sucesivamente.
    -- Universidad de Buitres Alternativos: Hace que el barco quede con una forma de saqueo compleja, donde una de las alternativas es la que el barco ya tenía, y se le agrega la forma "buitre" y la de cosas valiosas.
    -- Universidad Atlantica Inofensiva: No le afecta en absoluto.

ingresar_a_laboratorio :: Universidad -> Barco -> Barco
ingresar_a_laboratorio universidad barco = (perfil_academico universidad) barco

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
historia_inofensiva_para situaciones barcos = map fst (filter quedo_igual (barcos_antes_y_despues barcos situaciones))

barcos_antes_y_despues :: [Barco] -> [Situacion] -> [(Barco, Barco)]
barcos_antes_y_despues barcos situaciones = zip barcos (barcos_despues_de_situaciones barcos situaciones)

barcos_despues_de_situaciones :: [Barco] -> [Situacion] -> [Barco]
barcos_despues_de_situaciones barcos situaciones = map (aplicar_situaciones situaciones) barcos

quedo_igual :: (Barco, Barco) -> Bool
quedo_igual (barco_antes, barco_despues) = barco_antes == barco_despues

mas_tripulantes_despues_de_historia :: [Situacion] -> [Barco] -> Barco
mas_tripulantes_despues_de_historia situaciones barcos =  barco_mas_numeroso (barcos_despues_de_situaciones barcos situaciones)

barco_mas_numeroso :: [Barco] -> Barco
barco_mas_numeroso barcos = last (sortBy comparar_cantidad_tripulantes barcos)

comparar_cantidad_tripulantes :: Barco -> Barco -> Ordering
comparar_cantidad_tripulantes barco1 barco2 
  | (length (tripulacion barco1)) > (length (tripulacion barco2)) = GT
  | (length (tripulacion barco1)) < (length (tripulacion barco2)) = LT
  | otherwise = EQ
  

-- anclar_en_isla => itera para siempre
-- incorporar_a_tripulacion => no alcanza a incorporar el pirata nuevo (la función lo agrega al final)
-- abandonar_tripulacion => funciona - abandonar_tripulacion ((tripulacion mary_celeste) !! 1) mary_celeste
-- atacar_ciudad => cuelga
-- echar_piratas => funciona OK
-- abordar => falla
-- sacarle_todos_los_tesoros => si el infinito es ganador, funciona. Si es el perdedor, itera para siempre
