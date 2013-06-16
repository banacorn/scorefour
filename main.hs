module Main where 

import Game.Type
import Game.Host
import Game.Player

import Data.IORef
import Control.Monad
import System.Random
import Data.List (transpose)

import System.Environment (getArgs)

-- Arg serial playerID
data Arg = Arg (Int, Int) deriving (Eq, Show)

chunk n [] = []
chunk n list = take n list : chunk n (drop n list)

readBoard :: Int -> IO Game
readBoard playerID = do
    content <- readFile "chessboard.txt"
    let string = takeWhile (\c -> c /= '\r' && c /= '\n') content
    let transposed = concat . map concat . map transpose . map (chunk 4). transpose . chunk 4 $ string
    return . Game . map toChess $ transposed
    where   toChess '0' = Empty
            toChess '1' = if playerID == 1 then A else B
            toChess '2' = if playerID == 1 then B else A

writeAction :: Int -> Int -> IO ()
writeAction serial playerID = do

    game <- readBoard playerID
    print game

    --gen <- getStdGen
    --let (x, gen') = randomR (0, 3) gen :: (Int, StdGen)
    --let (y, gen'') = randomR (0, 3) gen' :: (Int, StdGen)

    let (x, y) = head . fst $ choose game testParameter
    print  $ show (succ serial) ++ " /drop " ++ show x ++ " " ++ show y
    writeFile "action.txt" $ show (succ serial) ++ " /drop " ++ show x ++ " " ++ show y
    --writeFile "action.txt" $ show (succ serial) ++ " /drop 0 0"
    --setStdGen gen'


parseRequest :: String -> Maybe Int
parseRequest string = case words string of
    [] -> Nothing
    (n:_) -> Just $ read n

readRequest :: IORef Arg -> IO ()
readRequest serialRef = do
    Arg (serial, playerID) <- readIORef serialRef
    checkRequest (readRequest serialRef) $ \serial' -> do
        case serial' > serial of
            -- read & write
            True -> do
                --print $ "#" ++ show serial' ++ " dropped"
                writeAction serial' playerID
                writeIORef serialRef $ Arg (succ serial', playerID)
                readRequest serialRef
            -- again
            False -> readRequest serialRef
    where   checkRequest t e = do
                content <- readFile "request.txt"
                case parseRequest content of
                    Nothing -> t
                    Just serial' -> e serial'


--main = do
--    playerID <- fmap (read . head) getArgs :: IO Int
--    serialRef <- newIORef $ Arg (0, playerID)
--    readRequest serialRef

main = do
    print $ choose m testParameter
