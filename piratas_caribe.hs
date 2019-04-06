import Data.List

data Pirata = Pirata
  { nombrePirata :: String
  , botin        :: [Tesoro]
  } deriving (Show, Eq)

data Tesoro = Tesoro
  { nombreTesoro :: String
  , valor        :: Integer
  } deriving (Show, Eq)


data Tripulacion = Tripulacion
   { miembros :: [Pirata]
   , barco    :: String} deriving (Show, Eq)

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

--BARCOS
perla = Tripulacion [jackSparrow, anneBonny] "Perla Negra"
holandes = Tripulacion [davidJones] "Holandes Errante"


--FUNCIONES
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

-- NAVEGANDO LOS SIETE MARES

incorporar_a_tripulacion :: Pirata -> Tripulacion -> Tripulacion
incorporar_a_tripulacion pirata tripulacion = Tripulacion ((miembros tripulacion) ++ [pirata]) (barco tripulacion)

abandonar_tripulacion :: Pirata -> Tripulacion -> Tripulacion
abandonar_tripulacion pirata tripulacion =  Tripulacion (delete pirata (miembros tripulacion )) (barco tripulacion)

saquear pirata forma tesoro 
  | forma tesoro = adquirir_tesoro pirata tesoro
  | otherwise = pirata

solo_tesoros_valiosos tesoro = (valor tesoro) > 100

solo_tesoros_especificos clave tesoro = (nombreTesoro tesoro) == clave

pirata_con_corazon tesoro = False

forma_compleja formas tesoro = any (==True) (map  ($tesoro) formas)