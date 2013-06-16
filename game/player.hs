module Game.Player where

import Game.Type
import Game.Stat

m = n =.= (0, 0)  =.= (0, 0)  =.= (0, 0)  =.= (0, 0) 


action :: Player -> Game -> Game
action _ game = game

replace :: [a] -> Int -> a -> [a]
replace [] n a = []
replace (x:xs) 0 a = a:xs
replace (x:xs) n a = x:replace xs (pred n) a

dropChess chess position (Game slots) = Game $ dropChess' slots chess position
    where   dropChess' [] chess (x, y) = []
            dropChess' slots chess (x, y)
                | firstLayer !! position == Empty = replace slots position chess
                | otherwise                       = firstLayer ++ (dropChess' (drop 16 slots) chess (x, y))
                where   firstLayer = take 16 slots
                        position = x + 4 * y

game =.= position = dropChess A position game
game =-= position = dropChess B position game

expand :: Game -> Chess -> [Game]
expand game chess = map (\ position -> dropChess chess position game ) (availableSlot game)

twice = map (flip expand B) $ expand n A



indexToPosition :: Int -> Position
indexToPosition n = (mod n 4, div n 4)


availableSlot :: Game -> ActionSequence
availableSlot (Game slots) = toPosition 0 . drop 48 $ slots 
    where   toPosition n []= []
            toPosition n (Empty:xs) = indexToPosition n : toPosition (succ n) xs
            toPosition n (_:xs) = toPosition (succ n) xs



--evaluateAction :: Player -> Game -> (Int, Int)