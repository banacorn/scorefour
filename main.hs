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


