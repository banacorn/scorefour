module Game.Stat (stat) where

import Game.Type
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


toIndex x y z = x + y * 4 + z * 16
{-# INLINE toIndex #-}

countConnection array = (value `div` 1000000, value `mod` 1000000 `div` 10000, value `mod` 10000 `div` 100, value `mod` 100)
    where
            value = countLines
            evaluate A = 100
            evaluate B = 1
            evaluate Empty = 0
            {-# INLINE evaluate #-}

            count 400 = 1000000
            count 4   = 100
            count 301 = 10000
            count 300 = 10000
            count 3   = 1
            count 103 = 1
            count _   = 0
            {-# INLINE count #-}

            allIndices = [
                    [0,1,2,3],[16,17,18,19],[32,33,34,35],[48,49,50,51],[4,5,6,7],[20,21,22,23],[36,37,38,39],[52,53,54,55],[8,9,10,11],[24,25,26,27],[40,41,42,43],[56,57,58,59],[12,13,14,15],[28,29,30,31],[44,45,46,47],[60,61,62,63],
                    [0,4,8,12],[16,20,24,28],[32,36,40,44],[48,52,56,60],[1,5,9,13],[17,21,25,29],[33,37,41,45],[49,53,57,61],[2,6,10,14],[18,22,26,30],[34,38,42,46],[50,54,58,62],[3,7,11,15],[19,23,27,31],[35,39,43,47],[51,55,59,63],
                    [0,16,32,48],[4,20,36,52],[8,24,40,56],[12,28,44,60],[1,17,33,49],[5,21,37,53],[9,25,41,57],[13,29,45,61],[2,18,34,50],[6,22,38,54],[10,26,42,58],[14,30,46,62],[3,19,35,51],[7,23,39,55],[11,27,43,59],[15,31,47,63],
                    [0,20,40,60],[1,21,41,61],[2,22,42,62],[3,23,43,63], -- XAC
                    [48,36,24,12],[49,37,25,13],[50,38,26,14],[51,39,27,15], -- XDC
                    [0,17,34,51],[4,21,38,55],[8,25,42,59],[12,29,46,63], -- YAC
                    [48,33,18,3],[52,37,22,7],[56,41,26,11],[60,45,30,15], -- YDC
                    [0,5,10,15],[16,21,26,31],[32,37,42,47],[48,53,58,63], -- ZAC
                    [12,9,6,3],[28,25,22,19],[44,41,38,35],[60,57,54,51], -- ZDC
                    [0,21,42,63],[3,22,41,60],[12,25,38,51],[15,26,37,48]
                ]
            countLines = sum [ count $ sum [ evaluate (array ! i) | i <- indices ]| indices <- allIndices ]
            {-# INLINE countLines #-}
