module Game.Host where

import Game.Type
import Game.Player 


-- instance of Game
emptyGame = Game $ replicate 64 Empty



--runRound :: (Parameter, Parameter) -> Game -> Game
--runRound (a, b) = 

--runRounds _ 0 = id
--runRounds players n = runRounds players (n - 1) . runRound players 

--runCompleteGame players = runRounds players 32 emptyGame
--    where   emptyGame = Game $ replicate 64 Empty




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


