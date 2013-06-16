module Game.Player where

import Game.Type
import Game.Stat
import Data.List (maximumBy)
import Data.Ord (comparing)

-- test data

m = n =.= (0, 0) =.= (0, 0) =.= (0, 0) =-= (0, 3)

full = Game $ replicate 63 B ++ [Empty]

testParameter = Parameter {
    scoreFourW = 3,
    openThreeW = 1,
    cornerAndCoreW = 0,
    surfaceW = 0,
    ratioW = -1
}

--playerA = Player A parameter
--playerB = Player B parameter


--twice = map (evaluatePair . flip expand B) $ expand n A
--    where   evaluatePair (a, b) = evaluate a + (evaluate b) * -1

--getRatio (Player _ parameters) = ratioW parameters

evaluate :: Parameter -> Stat -> Double
evaluate parameters stat = 
    fromIntegral (scoreFour       stat) * scoreFourW       parameters +
    fromIntegral (openThree       stat) * openThreeW       parameters +
    fromIntegral (cornerAndCore   stat) * cornerAndCoreW   parameters +
    fromIntegral (surface         stat) * surfaceW         parameters

evaluateAction :: Parameter -> Action -> ([Position], Double)
evaluateAction parameters (positions, game) = (positions, fitness)
    where   fitness = evaluate parameters statA + (evaluate parameters statB) * ratio 
            (statA, statB) = stat game
            ratio = ratioW parameters


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

expand :: Chess -> Action -> [Action]
expand chess (actions, game) = let tree = availableSlot game in
    case tree of
        [] -> [(actions, game)]
        tree -> map (\ position -> (actions ++ [position], dropChess chess position game)) tree

choose :: Game -> Parameter -> ([Position], Double)
choose game parameters = maximumBy (comparing snd) $ map (evaluateAction parameters) tree
    where   tree = expand A ([], game) >>= expand B >>= expand A >>= expand B

indexToPosition :: Int -> Position
indexToPosition n = (mod n 4, div n 4)

availableSlot :: Game -> [Position]
availableSlot (Game slots) = toPosition 0 . drop 48 $ slots 
    where   toPosition n []= []
            toPosition n (Empty:xs) = indexToPosition n : toPosition (succ n) xs
            toPosition n (_:xs) = toPosition (succ n) xs



--evaluateAction :: Player -> Game -> (Int, Int)