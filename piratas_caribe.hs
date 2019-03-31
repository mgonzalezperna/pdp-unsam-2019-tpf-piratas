data Pirata = Pirata {nombre :: String
                     ,botin :: Tesoro
                     } deriving (Show)

data Tesoro {nombre :: String
            ,valor :: Integer
            } deriving (Show)
