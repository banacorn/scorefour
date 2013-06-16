module Main where 

import Game.Type
import Game.Host
import Game.Player
import Game.Wrapper


main = print $ runCompleteGame testParameterPair
--main = do
--    print $ decide (Game $ replicate 64 Empty) A testParameter
