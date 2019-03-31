data Pirata = Pirata {nombrePirata :: String,
                     botin :: [Tesoro]
                     } deriving (Show)

data Tesoro = Tesoro {nombreTesoro :: String,
                     valor :: Integer
                     } deriving (Show)

juventud :: Tesoro
juventud = Tesoro { nombreTesoro = "Juventud",
                    valor = 20000
                  }

belleza :: Tesoro
belleza = Tesoro {nombreTesoro = "Belleza",
                valor = 3000
                }

viotti :: Pirata
viotti = Pirata { nombrePirata = "Viotti",
            botin = [juventud, belleza]
         }

cantidad_tesoros :: Pirata -> Int
cantidad_tesoros pirata = length (botin pirata)

es_afortunado :: Pirata -> Bool
es_afortunado pirata = sum (map valor (botin pirata)) > 10000
