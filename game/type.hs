module Game.Type where

import Data.Monoid
import Data.List (intersperse)

 --datatypes
data Chess = Empty | A | B deriving (Eq)
type Position = (Int, Int) 
type Action = ([Position], Game)
data Game = Game [Chess] deriving (Eq)
--data Player = Player Chess Parameter deriving (Eq, Show)
data Stat = Stat {
    scoreFour :: Int,
    openThree :: Int,
    cornerAndCore :: Int,
    surface :: Int
} deriving (Eq)

data Parameter = Parameter {
    scoreFourW :: Double,
    openThreeW :: Double,
    cornerAndCoreW :: Double,
    surfaceW :: Double,
    ratioW :: Double
} deriving (Eq, Show)

n = Game $ replicate 64 Empty


-- instaces
instance Show Chess where
    show Empty = " "
    show A = "O"
    show B = "X"

instance Show Game where
    show (Game slots) = "\n" ++ allLine 0 ++ allLine 1 ++ allLine 2 ++ allLine 3
        where   allLine n = "| " ++ line n ++ " | " ++ line (n + 4) ++ " | " ++ line (n + 8) ++ " | " ++ line (n + 12) ++ " |\n"
                line n = intersperse ' ' . concat . map show . take 4 . drop (n * 4) $ slots

instance Show Stat where
    show stat = "\n **** " ++ show (scoreFour stat) ++ 
        "\n ***_ " ++ show (openThree stat) ++
        "\n corn " ++ show (cornerAndCore stat) ++
        "\n surf " ++ show (surface stat) ++
        "\n"

instance Monoid Stat where
    mempty = Stat { scoreFour = 0, openThree = 0, cornerAndCore = 0, surface = 0 }
    a `mappend` b = Stat {
            scoreFour = scoreFour a + scoreFour b,
            openThree = openThree a + openThree b,
            cornerAndCore = cornerAndCore a + cornerAndCore b,
            surface = surface a + surface b
        }