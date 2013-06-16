module Main where 

import Game.Type
import Game.Host

import Data.IORef
import Control.Monad
import System.Random


readBoard :: IO Game
readBoard = do
    content <- readFile "chessboard.txt"
    let string = takeWhile (\c -> c /= '\r' && c /= '\n') content
    return . Game . map toChess $ string
    where   toChess '0' = Empty
            toChess '1' = A
            toChess '2' = B

writeAction :: Int -> IO ()
writeAction serial = do 
    gen <- getStdGen
    let (x, gen') = randomR (0, 3) gen :: (Int, StdGen)
    let (y, gen'') = randomR (0, 3) gen' :: (Int, StdGen)
    writeFile "action.txt" $ show (succ serial) ++ " /drop " ++ show x ++ " " ++ show y
    setStdGen gen'


parseRequest :: String -> Maybe Int
parseRequest string = case words string of
    [] -> Nothing
    (n:_) -> Just $ read n

readRequest :: IORef (Int, Int) -> IO ()
readRequest serialRef = do
    (serial, number) <- readIORef serialRef
    checkRequest number (readRequest serialRef) $ \serial' -> do
        case serial' > serial of
            True -> do
                print $ "#" ++ show serial' ++ " " ++ show number ++ " dropped"
                writeAction serial'
                writeIORef serialRef (succ serial', succ number)
                readRequest serialRef
            False -> readRequest serialRef
    where   printDone = do
                print "done"
                getLine
                print "done"

            checkRequest number t e = do
                if number > 100 then printDone else do
                    content <- readFile "request.txt"
                    case parseRequest content of
                        Nothing -> t
                        Just serial' -> e serial'


main = do
    serialRef <- newIORef (0, 0)
    readRequest serialRef



---- instance of Game
--n = Game $ replicate 64 Empty
--f = Game $ replicate 64 A
--line1 = go A (0, 0) n >>= go A (1, 1) >>= go A (1, 1) >>= go A (2, 2) >>= go A (2, 2) >>= go A (2, 2) >>= go A (3, 3) >>= go A (3, 3) >>= go A (3, 3) >>= go A (3, 3)

---- utilities
--replace [] a n = []
--replace (_:xs) a 0 = a:xs
--replace (x:xs) a n = x : replace xs a (pred n)

--curry3 f a b c = f (a,b,c)

--inspect (Game slots) (x, y, z) = slots !! (z * 16 + y * 4 + x)
--slotsAvailable game (x, y) = length $ takeWhile (== Empty) $ map (\z -> inspect game (x, y, z)) [3, 2, 1, 0]

--go player (x, y) game = case slotsAvailable game (x, y) of
--    0 -> Nothing
--    n -> Just . Game $ replace slots player (x + 4 * y + (4 - n) * 16)
--    where   Game slots = game

--countLine player game = countCross + countX + countY + countZ + countXAscCross + countXDesCross + countYAscCross + countYDesCross + countZAscCross + countZDesCross
--    where   inspect' = inspect game
--            connected = all ((==) player)
--            toNum True = 1
--            toNum False = 0
--            count = sum . map toNum
--            countX = count [connected (map (\x -> inspect' (x, y, z)) [0..3]) | y <- [0..3], z <- [0..3]]
--            countY = count [connected (map (\y -> inspect' (x, y, z)) [0..3]) | x <- [0..3], z <- [0..3]]
--            countZ = count [connected (map (\z -> inspect' (x, y, z)) [0..3]) | x <- [0..3], y <- [0..3]]
--            countXAscCross = count [ connected [ inspect' (x, n, n) | n <- [0..3] ] | x <- [0..3] ]
--            countXDesCross = count [ connected [ inspect' (x, n, 3 - n) | n <- [0..3] ] | x <- [0..3] ]
--            countYAscCross = count [ connected [ inspect' (n, y, n) | n <- [0..3] ] | y <- [0..3] ]
--            countYDesCross = count [ connected [ inspect' (n, y, 3 - n) | n <- [0..3] ] | y <- [0..3] ]
--            countZAscCross = count [ connected [ inspect' (n, n, z) | n <- [0..3] ] | z <- [0..3] ]
--            countZDesCross = count [ connected [ inspect' (n, 3 - n, z) | n <- [0..3] ] | z <- [0..3] ]
--            countCross = count [ 
--                connected [ inspect' (n, n, n) | n <- [0..3] ], 
--                connected [ inspect' (3 - n, n, n) | n <- [0..3] ], 
--                connected [ inspect' (n, 3 - n, n) | n <- [0..3] ],
--                connected [ inspect' (3 - n, 3 - n, n) | n <- [0..3] ]
--                ]




