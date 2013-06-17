module Game.Host (runCompleteGame) where

import Game.Type
import Game.Player 
import Game.Stat 

-- instance of Game
emptyGame = Game $ replicate 64 Empty
dryRound = oneRound (testParameter, testParameter) emptyGame
dryRounds n = manyRound (testParameter, testParameter) n emptyGame

run :: Chess -> Parameter -> Game -> Game
run chess parameter game = dropChess chess decision game
    where   decision = decide game chess parameter

oneRound :: (Parameter, Parameter) -> Game -> Game
oneRound (a, b) = run B b . run A a 

manyRound :: (Parameter, Parameter) -> Int -> Game -> Game
manyRound _ 0 = id
manyRound parameters n = manyRound parameters (n - 1) . oneRound parameters 

runCompleteGame :: (Parameter, Parameter) -> (Int, Int)
--runCompleteGame parameters = (fitness statA, fitness statB)
runCompleteGame parameters = (fitness statA + fitness statA', fitness statB + fitness statB')
    where   (statA, statB) = stat $ manyRound parameters 32 emptyGame
            (statB', statA') = stat $ manyRound (swap parameters) 32 emptyGame
            swap (a, b) = (b, a)

fitness :: Stat -> Int
fitness s = score connections
    where   connections = scoreFour s
            score 0 = 0
            score 1 = 5
            score 2 = 8
            score 3 = 10
            score n = 1 + score (pred n)

--f = Game $ replicate 64 A
--line1 = go A (0, 0) n >>= go A (1, 1) >>= go A (1, 1) >>= go A (2, 2) >>= go A (2, 2) >>= go A (2, 2) >>= go A (3, 3) >>= go A (3, 3) >>= go A (3, 3) >>= go A (3, 3)

---- utilities
--replace [] a n = []
--replace (_:xs) a 0 = a:xs
--replace (x:xs) a n = x : replace xs a (pred n)

--slotsAvailable game (x, y) = length $ takeWhile (== Empty) $ map (\z -> inspect game (x, y, z)) [3, 2, 1, 0]
--go player (x, y) game = case slotsAvailable game (x, y) of
--    0 -> Nothing
--    n -> Just . Game $ replace slots player (x + 4 * y + (4 - n) * 16)
--    where   Game slots = game
--inspect (Game slots) (x, y, z) = slots !! (z * 16 + y * 4 + x)


