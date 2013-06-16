module Game.Type where
 --datatypes
data Chess = Empty | A | B deriving (Eq)
data Game = Game [Chess] deriving (Eq)

-- instaces
instance Show Chess where
    show Empty = " "
    show A = "O"
    show B = "X"

instance Show Game where
    show (Game slots) = "\n" ++ allLine 0 ++ allLine 1 ++ allLine 2 ++ allLine 3
        where   allLine n = line n ++ "|" ++ line (n + 4) ++ "|" ++ line (n + 8) ++ "|" ++ line (n + 12) ++ "\n"
                line n = concat $ map show $ drop (n * 4) $ take (n * 4 + 4) $ slots
