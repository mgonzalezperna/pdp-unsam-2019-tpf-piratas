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

zapatillas :: Tesoro
zapatillas = Tesoro {nombreTesoro = "Zapatillas Mike",
                valor = 400
                }

biciCopada :: Tesoro
biciCopada = Tesoro{nombreTesoro = "Bicicleta GT Avalanche"
                   valor = 25000
                   }

zapatillas :: Tesoro
zapatillas = Tesoro {nombreTesoro = "Zapatillas Mike",
                valor = 500
                }

viotti :: Pirata
viotti = Pirata { nombrePirata = "Viotti el terrible",
            botin = [juventud, belleza]
         }

dini :: Pirata
dini = Pirata { nombrePirata = "Dini el magnifico",
            botin = []
              }

cantidad_tesoros :: Pirata -> Int
cantidad_tesoros pirata = length (botin pirata)

es_afortunado :: Pirata -> Bool
es_afortunado pirata = sum (map valor (botin pirata)) > 10000


