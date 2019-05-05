import           Data.List
import           Text.Show.Functions

data Pirata = Pirata
  { nombrePirata :: String
  , botin        :: [Tesoro]
  } deriving (Show, Eq)

data Tesoro = Tesoro
  { nombreTesoro :: String
  , valor        :: Double 
  } deriving (Show, Eq)

data Barco = Barco
  { tripulacion  :: [Pirata]
  , nombreBarco  :: String
  , forma_saqueo :: Tesoro -> Bool
  } deriving (Show)

data Isla = Isla
  { elemento_tipico :: Tesoro
  , nombreIsla      :: String
  } deriving (Show, Eq)

data Ciudad = Ciudad
  { tesoros_disponibles :: [Tesoro]
  , nombreCiudad        :: String
  } deriving (Show, Eq)

data Pais = Pais
    { nombre_pais :: String
    , tasa_segun_pais :: Double
    }

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

--PAISES

argentina :: Pais
argentina = Pais {nombre_pais = "Argentina", tasa_segun_pais = 0.74}

mexico :: Pais 
mexico = Pais {nombre_pais = "Mexico", tasa_segun_pais = 0.34}

brasil :: Pais
brasil = Pais {nombre_pais = "Brasil", tasa_segun_pais = 0.12}

paises :: [Pais]
paises = [argentina, mexico, brasil]

--TESORO Bonos en dafault

bonos_en_dafault :: Double -> Double -> Tesoro
bonos_en_dafault minima_cotizacion maxima_cotizacion = Tesoro { nombreTesoro = "Bono", valor = valor_bono minima_cotizacion maxima_cotizacion}

valor_bono :: Double -> Double -> Double
valor_bono minima_cotizacion maxima_cotizacion = (1.5 *) $(abs((-) minima_cotizacion maxima_cotizacion))

--TESORO Letras de liquidez

letras_de_liquidez :: Double -> String ->Tesoro
letras_de_liquidez valor_nominal nombre_pais = Tesoro { nombreTesoro = "LeLiq " + nombre_pais, valor = valor_letras valor_nominal nombre_pais}

valor_letras :: Double -> String -> Double
valor_letras valor_nominal nombre_pais = (tasa_del_pais nombre_pais) * valor_nominal

tasa_del_pais :: String -> Double
tasa_del_pais nombre
  | elem nombre (map(nombre_pais) paises) = tasa_segun_pais(filter (==nombre)(map(nombre_pais) paises))
  | otherwise = -1

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
  Ciudad {tesoros_disponibles = [oro], nombreCiudad = "Carmen de Patagones"}

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
adquirir_tesoro pirata tesoro =
  -- Pirata (nombrePirata pirata) (tesoro : (botin pirata))
  pirata{botin = tesoro: botin pirata}

perder_tesoros_valiosos :: Pirata -> Pirata -- se puede delegar qué cosa es un tesoro valioso, y usarlo acá
perder_tesoros_valiosos pirata =
  -- Pirata (nombrePirata pirata) (filter ((< 100) . valor) (botin pirata))
  pirata { botin = (filter ((< 100) . valor) (botin pirata)) }

perder_tesoros_con_nombre :: String -> Pirata -> Pirata -- tesoros con nombre = tesoros específicos
perder_tesoros_con_nombre nombre pirata = pirata { botin = (filter ((/= nombre) . nombreTesoro) (botin pirata)) }
  -- Pirata
  --   (nombrePirata pirata)
  --   (filter ((/= nombre) . nombreTesoro) (botin pirata))

--TEMPORADA DE SAQUEOS
saquear :: Pirata -> (Tesoro -> Bool) -> Tesoro -> Pirata
saquear pirata forma tesoro
  | forma tesoro = adquirir_tesoro pirata tesoro
  | otherwise = pirata

solo_tesoros_valiosos :: Tesoro -> Bool -- y reutilizar tesoro valioso acá.
solo_tesoros_valiosos = (> 100) . valor

solo_tesoros_especificos :: String -> Tesoro -> Bool -- tesoros con nombre = tesoros específicos
solo_tesoros_especificos clave = (== clave) . nombreTesoro

pirata_con_corazon :: Tesoro -> Bool
pirata_con_corazon tesoro = False

--Condicion de cumplimiento del any
evaluar :: Tesoro -> (Tesoro -> Bool) -> Bool
evaluar tesoro forma = forma tesoro

forma_compleja :: [(Tesoro -> Bool)] -> Tesoro -> Bool
forma_compleja formas tesoro = any (evaluar tesoro) formas

-- NAVEGANDO LOS SIETE MARES
incorporar_a_tripulacion :: Pirata -> Barco -> Barco
incorporar_a_tripulacion pirata barco = barco { tripulacion = (tripulacion barco) ++ [pirata] }
  -- Barco
  --   ((tripulacion barco) ++ [pirata])
  --   (nombreBarco barco)
  --   (forma_saqueo barco)

abandonar_tripulacion :: Pirata -> Barco -> Barco
abandonar_tripulacion pirata barco = barco { tripulacion = delete pirata (tripulacion barco) }
  -- Barco
  --   (delete pirata (tripulacion barco))
  --   (nombreBarco barco)
  --   (forma_saqueo barco)

anclar_en_isla :: Barco -> Isla -> Barco
anclar_en_isla barco isla = barco { tripulacion = tomar_tesoros (tripulacion barco) (elemento_tipico isla) }
  -- Barco
  --   (tomar_tesoros (tripulacion barco) (elemento_tipico isla))
  --   (nombreBarco barco)
  --   (forma_saqueo barco)

tomar_tesoros :: [Pirata] -> Tesoro -> [Pirata]
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
saquear_ciudad barco ciudad = barco { tripulacion = (obtener_tesoros_por_tripulante (forma_saqueo barco) (tripulacion barco) (tesoros_disponibles ciudad)) }
  -- Barco
  --   (obtener_tesoros_por_tripulante (forma_saqueo barco) (tripulacion barco) (tesoros_disponibles ciudad) )
  --   (nombreBarco barco)
  --   (forma_saqueo barco)

obtener_tesoros_por_tripulante :: (Tesoro -> Bool) -> [Pirata] -> [Tesoro] -> [Pirata]
obtener_tesoros_por_tripulante formaRobarGanador tripulacionGanador tesorosPerdedor = map (tomar_si_le_interesa (formaRobarGanador)) (zip tripulacionGanador tesorosPerdedor)

tomar_si_le_interesa :: (Tesoro -> Bool) -> (Pirata, Tesoro) -> Pirata
tomar_si_le_interesa forma (pirata, tesoro) = saquear pirata forma tesoro

echar_piratas :: Barco -> Int -> Barco
echar_piratas barco quedan = barco { tripulacion = (take quedan (tripulacion barco)) }
  -- Barco
  --   (take quedan (tripulacion barco))
  --   (nombreBarco barco)
  --   (forma_saqueo barco)

mas_tesoros_que_tripulantes :: [Tesoro] -> [Pirata] -> Bool
mas_tesoros_que_tripulantes tesoros tripulantes =
  (length tesoros) >= (length tripulantes)

abordar :: Barco -> Barco -> (Barco, Barco)
abordar barco1 barco2 
  | length (tripulacion barco1) >= length (tripulacion barco2) = sacarle_todos_los_tesoros barco1 barco2
  | otherwise = sacarle_todos_los_tesoros barco2 barco1

sacarle_todos_los_tesoros :: Barco -> Barco -> (Barco, Barco)
sacarle_todos_los_tesoros ganador perdedor = 
  (ganador { tripulacion = (obtener_tesoros_por_tripulante (solo_tesoros_valiosos) (tripulacion ganador) (todos_los_tesoros (tripulacion perdedor))) }
  ,perdedor { tripulacion = (map perder_tesoros_valiosos (tripulacion perdedor)) })

  -- ( Barco
  --     (obtener_tesoros_por_tripulante (solo_tesoros_valiosos) (tripulacion ganador) (todos_los_tesoros (tripulacion perdedor)) )
  --     (nombreBarco ganador)
  --     (forma_saqueo ganador)
  -- , Barco
  --     (map perder_tesoros_valiosos (tripulacion perdedor))
  --     (nombreBarco perdedor)
  --     (forma_saqueo perdedor))


todos_los_tesoros :: [Pirata] -> [Tesoro]      
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

super_pelicula :: Barco -> Barco -> Isla -> Isla -> Ciudad -> Ciudad -> (Barco, Barco)
super_pelicula barco barco2 isla isla2 ciudad ciudad2 = abordar (atacar_ciudad (anclar_en_isla barco isla) ciudad) (atacar_ciudad(anclar_en_isla barco2 isla2) ciudad2)



