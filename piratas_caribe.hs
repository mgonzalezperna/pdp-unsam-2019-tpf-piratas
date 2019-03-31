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

comparar_nombres_tesoro :: Tesoro -> Tesoro -> Bool
comparar_nombres_tesoro tesoro_1 tesoro_2 = nombreTesoro tesoro_1 == nombreTesoro tesoro_2


