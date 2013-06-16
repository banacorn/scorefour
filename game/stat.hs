module Game.Stat (stat) where

import Game.Type
import Data.List (foldl')
import Data.Array.Unboxed

stat :: Game -> (Stat, Stat)
stat game = (statA, statB)
    where 
            (cca, ccb) = countCornerAndCore array
            (sfa, sfb) = countSurface array
            (fa, ta, fb, tb) = countConnection array
            statA = Stat {
                scoreFour = fa,
                openThree = ta,
                cornerAndCore = cca,
                surface = sfa
            }
            statB = Stat {
                scoreFour = fb,
                openThree = tb,
                cornerAndCore = ccb,
                surface = sfb
            } 
            array = toArray game

toArray :: Game -> Array Int Chess
toArray (Game slots) = listArray (0, 63) slots

fromArray :: Array Int Chess -> Game
fromArray = Game . elems

cornerAndCoreIndices = [0,3,12,15,21,22,25,26,37,38,41,42,48,51,60,63]
surfaceIndices = [1,2,4,5,6,7,8,9,10,11,13,14,16,17,18,19,20,23,24,27,28,29,30,31,32,33,34,35,36,39,40,43,44,45,46,47,49,50,52,53,54,55,56,57,58,59,61,62]

countCornerAndCore :: Array Int Chess -> (Int, Int)
countCornerAndCore array = (length $ filter (== A) list, length $ filter (== B) list)
    where   list = [ array ! i | i <- cornerAndCoreIndices]
countSurface :: Array Int Chess -> (Int, Int)
countSurface array = (length $ filter (== A) list, length $ filter (== B) list)
    where   list = [ array ! i | i <- surfaceIndices]



countConnection array = (value `div` 1000000, value `mod` 1000000 `div` 10000, value `mod` 10000 `div` 100, value `mod` 100)
    where
            value = countX + countY + countZ + countXAC + countXDC + countYAC + countYDC + countZAC + countZDC + countCross
            toIndex x y z = x + y * 4 + z * 16
            evaluate A = 100
            evaluate B = 1
            evaluate Empty = 0
            count line
                | value == 400 = 1000000
                | value == 4 = 100
                | value == 301 = 10000
                | value == 300 = 10000
                | value == 3 = 1
                | value == 103 = 1
                | otherwise = 0
                where   value = foldl' (\acc e -> acc + evaluate e) 0 line
            countX      = sum [ count [ array ! (toIndex x y z) | x <- [0..3] ] | y <- [0..3], z <- [0..3]]
            countY      = sum [ count [ array ! (toIndex x y z) | y <- [0..3] ] | x <- [0..3], z <- [0..3]]
            countZ      = sum [ count [ array ! (toIndex x y z) | z <- [0..3] ] | x <- [0..3], y <- [0..3]]
            countXAC    = sum [ count [ array ! (toIndex i n n)     | n <- [0..3] ] | i <- [0..3] ]
            countXDC    = sum [ count [ array ! (toIndex i n (3-n)) | n <- [0..3] ] | i <- [0..3] ]
            countYAC    = sum [ count [ array ! (toIndex n i n)     | n <- [0..3] ] | i <- [0..3] ]
            countYDC    = sum [ count [ array ! (toIndex n i (3-n)) | n <- [0..3] ] | i <- [0..3] ]
            countZAC    = sum [ count [ array ! (toIndex n n i)     | n <- [0..3] ] | i <- [0..3] ]
            countZDC    = sum [ count [ array ! (toIndex n (3-n) i) | n <- [0..3] ] | i <- [0..3] ]
            countCross  = sum [
                    count [ array ! (toIndex n     n     n) | n <- [0..3] ],
                    count [ array ! (toIndex (3-n) n     n) | n <- [0..3] ],
                    count [ array ! (toIndex n     (3-n) n) | n <- [0..3] ],
                    count [ array ! (toIndex (3-n) (3-n) n) | n <- [0..3] ]
                ]