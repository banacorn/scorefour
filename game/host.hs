module Game.Host where

import Game.Type
import Game.Player


---- instance of Game
--n = Game $ replicate 64 Empty
--f = Game $ replicate 64 A
--line1 = go A (0, 0) n >>= go A (1, 1) >>= go A (1, 1) >>= go A (2, 2) >>= go A (2, 2) >>= go A (2, 2) >>= go A (3, 3) >>= go A (3, 3) >>= go A (3, 3) >>= go A (3, 3)

---- utilities
replace [] a n = []
replace (_:xs) a 0 = a:xs
replace (x:xs) a n = x : replace xs a (pred n)

slotsAvailable game (x, y) = length $ takeWhile (== Empty) $ map (\z -> inspect game (x, y, z)) [3, 2, 1, 0]
go player (x, y) game = case slotsAvailable game (x, y) of
    0 -> Nothing
    n -> Just . Game $ replace slots player (x + 4 * y + (4 - n) * 16)
    where   Game slots = game
inspect (Game slots) (x, y, z) = slots !! (z * 16 + y * 4 + x)


score 0 = 0
score 1 = 5
score 2 = 3
score 3 = 2
score n = 1 + score (n - 1)

countLine player game = countCross + countX + countY + countZ + countXAscCross + countXDesCross + countYAscCross + countYDesCross + countZAscCross + countZDesCross
    where   inspect' = inspect game
            connected = all ((==) player)
            toNum True = 1
            toNum False = 0
            count = sum . map toNum
            countX = count [connected (map (\x -> inspect' (x, y, z)) [0..3]) | y <- [0..3], z <- [0..3]]
            countY = count [connected (map (\y -> inspect' (x, y, z)) [0..3]) | x <- [0..3], z <- [0..3]]
            countZ = count [connected (map (\z -> inspect' (x, y, z)) [0..3]) | x <- [0..3], y <- [0..3]]
            countXAscCross = count [ connected [ inspect' (x, n, n) | n <- [0..3] ] | x <- [0..3] ]
            countXDesCross = count [ connected [ inspect' (x, n, 3 - n) | n <- [0..3] ] | x <- [0..3] ]
            countYAscCross = count [ connected [ inspect' (n, y, n) | n <- [0..3] ] | y <- [0..3] ]
            countYDesCross = count [ connected [ inspect' (n, y, 3 - n) | n <- [0..3] ] | y <- [0..3] ]
            countZAscCross = count [ connected [ inspect' (n, n, z) | n <- [0..3] ] | z <- [0..3] ]
            countZDesCross = count [ connected [ inspect' (n, 3 - n, z) | n <- [0..3] ] | z <- [0..3] ]
            countCross = count [ 
                connected [ inspect' (n, n, n) | n <- [0..3] ], 
                connected [ inspect' (3 - n, n, n) | n <- [0..3] ], 
                connected [ inspect' (n, 3 - n, n) | n <- [0..3] ],
                connected [ inspect' (3 - n, 3 - n, n) | n <- [0..3] ]
                ]




