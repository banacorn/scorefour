module Game.Wrapper (start) where

import Data.List (transpose)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Data.IORef


import Game.Type
import Game.Player

-- Arg serial chess
data Arg = Arg (Int, Chess) deriving (Eq, Show)

chunk n [] = []
chunk n list = take n list : chunk n (drop n list)

readBoard :: Chess -> IO Game
readBoard chess = do
    content <- readFile "chessboard.txt"
    writeFile "log.txt" "reading chessboard.txt"
    let string = takeWhile (\c -> c /= '\r' && c /= '\n') content
    let transposed = concat . map concat . map transpose . map (chunk 4). transpose . chunk 4 $ string
    return . Game . map toChess $ transposed
    where   toChess '0' = Empty
            toChess '1' = A -- if chess == A then A else B
            toChess '2' = B -- if chess == A then B else A

parameters = Parameter {
    scoreFourW = 27.61,
    openThreeW = 9.68,
    cornerAndCoreW = 0.1766,
    surfaceW = -2.85,
    ratioW = -0.2273
}

writeAction :: Int -> Chess -> IO ()
writeAction serial chess = do
    game <- readBoard chess
    print game
    let (x, y) = decide game chess parameters
    print $ show (x, y)
    writeFile "action.txt" $ show (succ serial) ++ " /drop " ++ show x ++ " " ++ show y

parseRequest :: String -> Maybe Int
parseRequest string = case words string of
    [] -> Nothing
    (n:_) -> Just $ read n

readRequest :: IORef Arg -> IO ()
readRequest serialRef = do
    Arg (serial, chess) <- readIORef serialRef
    checkRequest (readRequest serialRef) $ \serial' -> do
        case serial' > serial of
            -- read & write
            True -> do
                writeAction serial' chess
                writeIORef serialRef $ Arg (succ serial', chess)
                readRequest serialRef
            -- again
            False -> readRequest serialRef
    where   checkRequest t e = do
                --threadDelay $ 1000000 * (60 - secs)
                fileExist <- doesFileExist "request.txt"
                if fileExist then do
                    content <- readFile "request.txt"
                    case parseRequest content of
                        Nothing -> t
                        Just serial' -> e serial'
                else checkRequest t e

start = do
    chess <- fmap (toChess . read . head) getArgs
    serialRef <- newIORef $ Arg (0, chess)
    writeFile "log.txt" "reading arg"
    readRequest serialRef
    where   toChess 1 = A
            toChess 2 = B
