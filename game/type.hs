module Game.Type where

import Data.List (intersperse)

 --datatypes
data Chess = Empty | A | B deriving (Eq)
type Position = (Int, Int) 
type ActionSequence = [Position]
data Game = Game [Chess] deriving (Eq)
data Player = Player deriving (Eq)

-- instaces
instance Show Chess where
    show Empty = " "
    show A = "O"
    show B = "X"

instance Show Game where
    show (Game slots) = "\n" ++ allLine 0 ++ allLine 1 ++ allLine 2 ++ allLine 3
        where   allLine n = "| " ++ line n ++ " | " ++ line (n + 4) ++ " | " ++ line (n + 8) ++ " | " ++ line (n + 12) ++ " |\n"
                line n = intersperse ' ' . concat . map show . drop (n * 4) . take (n * 4 + 4) $ slots
