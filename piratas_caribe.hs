data Pirata = Pirata {nombrePirata :: String,
                     botin :: [Tesoro]
                     } deriving (Show)

data Tesoro = Tesoro {nombreTesoro :: String,
                     valor :: Integer
                     } deriving (Show)

auricularesChetos :: Tesoro
auricularesChetos = Tesoro { nombreTesoro = "Auriculares Shure SRH 440",
                    valor = 6000
                  }

zapatillasViotti :: Tesoro
zapatillasViotti = Tesoro {nombreTesoro = "Zapatillas Mike",
                valor = 400
                }

zapatillasDini :: Tesoro
zapatillasDini = Tesoro {nombreTesoro = "Zapatillas Mike",
                valor = 500
                }

biciCopada :: Tesoro
biciCopada = Tesoro{nombreTesoro = "Bicicleta GT Avalanche",
                   valor = 25000
                   }

viotti :: Pirata
viotti = Pirata { nombrePirata = "Viotti el terrible",
            botin = [auricularesChetos, zapatillasViotti]
         }

dini :: Pirata
dini = Pirata { nombrePirata = "Dini el magnifico",
            botin = [biciCopada, zapatillasDini]
              }

cantidad_tesoros :: Pirata -> Int
cantidad_tesoros pirata = length (botin pirata)

es_afortunado :: Pirata -> Bool
es_afortunado pirata = sum (map valor (botin pirata)) > 10000

comparar_nombres_tesoros :: Tesoro -> Tesoro -> Bool
comparar_nombres_tesoros tesoro_1 tesoro_2 = nombreTesoro tesoro_1 == nombreTesoro tesoro_2

comparar_valores_tesoros :: Tesoro -> Tesoro -> Bool
comparar_valores_tesoros tesoro_1 tesoro_2 = valor tesoro_1 /= valor tesoro_2

comparar_valores_de_nombres_iguales :: Tesoro -> Tesoro -> Bool
comparar_valores_de_nombres_iguales tesoro_1 tesoro_2 = comparar_nombres_tesoros tesoro_1 tesoro_2 && 
            comparar_valores_tesoros tesoro_1 tesoro_2

cumpleCondicion :: [Tesoro] -> Tesoro -> Bool
cumpleCondicion botin tesoro = any (comparar_valores_de_nombres_iguales tesoro) botin

tienen_mismo_tesoro_y_valor_diferente :: Pirata -> Pirata -> Bool
tienen_mismo_tesoro_y_valor_diferente pirata_1 pirata_2 = any (cumpleCondicion (botin pirata_2)) (botin pirata_1)
