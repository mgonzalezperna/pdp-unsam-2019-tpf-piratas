data Pirata = Pirata {nombrePirata :: String,
                     botin :: Tesoro
                     } deriving (Show)

data Tesoro = Tesoro {nombreTesoro :: String,
                     valor :: Integer
                     } deriving (Show)


