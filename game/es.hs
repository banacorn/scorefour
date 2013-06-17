module Game.ES where

import Game.Type
import Game.Host

import Data.List (sort)
import Control.Monad (forever, replicateM, replicateM_)
--import Data.List (intercalate)
import qualified Control.Monad.Primitive as Prim
import qualified System.Random.MWC as MWC
import System.Random.MWC.Distributions (normal)
--import Data.Vector (singleton)
import GHC.Word (Word32)

testParent = Parameter {
    scoreFourW = 30,
    openThreeW = 10,
    cornerAndCoreW = 0,
    surfaceW = 0,
    ratioW = 0
}

-- 26 12 -3 -5 1.73
-- Vector [27.61996594433738,9.681497024876126,0.17666472202049996,-2.8568311242010305,-0.22734636519156462] 9.910481551887473e-2 (5,1000)

type Seed = MWC.Seed
type Gen = MWC.Gen (Prim.PrimState IO)

type Sigma = Double
data Genotype = Vector [Double] Sigma (Int, Int) deriving (Eq, Show)

tau = 0.7071
boundary = 0.005
a = 0.9

encode :: Parameter -> Genotype
encode parameter = Vector [
    scoreFourW parameter, openThreeW parameter, cornerAndCoreW parameter, surfaceW parameter,
    ratioW parameter
    ] 1 (0, 0)

decode :: Genotype -> Parameter
decode (Vector [a, b, c, d, e] sigma tags) = Parameter {
    scoreFourW = a,
    openThreeW = b,
    cornerAndCoreW = c,
    surfaceW = d,
    ratioW = e
}



mutate (Vector variables sigma (success, total), gen) = do

    --let successRate = fromIntegral success / fromIntegral total
    --print $ "success rate: " ++ show successRate
    --let sigma' = changeSigma sigma successRate
    --print $ "sigma +- : " ++ show sigma'


    variances <- replicateM (length variables) (normal 0 1 gen)
    let variables' = zipWith (\x v -> x + sigma * v) variables variances

    return (Vector variables' sigma (success, total), gen)
    --where   changeSigma sigma successRate
    --            | successRate > 0.2 = sigma / a
    --            | successRate < 0.2 = sigma * a
    --            | otherwise = sigma
    --        a = 0.9


--mutate :: (Genotype, Gen) -> IO (Genotype, Gen)
--mutate (Vector variables sigmas, gen) = do

--    shared <- normal 0 1 gen
--    norms <- replicateM (length variables) (normal 0 1 gen)
--    let sigmas' = zipWith (\original norm -> max boundary (original * exp (tau * shared + tau * norm))) sigmas norms
    
--    -- x' = x + Si x Ni(0, 1)
--    let variables' = zipWith3 (\x sigma norm-> x + sigma * norm) variables sigmas' norms
--    return (Vector variables' sigmas', gen)

instance Ord Parameter where
    compare a b = compare a' b'
        where   (a', b') = runCompleteGame (a, b)

instance Ord Genotype where
    compare a b = compare a' b'
        where   (a', b') = runCompleteGame (decode a, decode b)


-- (1 + 1)-1/5-ES
generate (father, gen) = do
    (child, gen') <- mutate (father, gen)
    let result = case child < father of
            True -> changeSigma True child
            False -> changeSigma False father
    print result
    return (result, gen')
    where   changeSigma successed (Vector v s (success, total)) = Vector v (change s) (success', succ total)
                where   success' = if successed then succ success else success
                        successRate = fromIntegral success / fromIntegral total
                        change sigma
                            | successRate > 0.2 && sigma < 10 = sigma / a
                            | successRate < 0.2 && sigma > 0.1 = sigma * a
                            | otherwise = sigma
                        a = 0.99
        --tagSuccess (Vector v s (success, total)) = if Vector v s (succ success, succ total)
        --tagFailure (Vector v s (success, total)) = Vector v s (     success, succ total)


 --(1, 7)-ES
--generate :: (Genotype, Gen) -> IO (Genotype, Gen)
--generate (vector, gen) = do
--    (vectors', gen') <- mapCollectM mutate (parents, gen)
--    return (maximum $ vectors', gen')
--    where   
--            generateSize = 7
--            parents = replicate generateSize vector

----iterateM :: (Monad m) => (a -> m a) -> Int -> a -> m a
--iterateM _ 0 a = return a 
--iterateM f n a = do
--    (a', g') <- f a
--    print a'
--    iterateM f (n - 1) (a', g')

mapCollectM :: (Monad m) => ((a, g) -> m (a, g)) -> ([a], g) -> m ([a], g)
mapCollectM _ ([], g) = return ([], g)
mapCollectM f ((a:as), g) = do
    (a', g') <- f (a, g)
    (as', g'') <- mapCollectM f (as, g')
    return (a':as', g'')

--run1 = do
--    print "== start =="
--    gen <- MWC.create :: IO Gen
--    --(vectors, gen') <- generate 100 (map encode testParents, gen)
--    (vectors, gen') <- iterateM generate 100 (encode testParent, gen)
--    print vectors


go  = do
    gen <- MWC.create :: IO Gen
    runEinFuenftel 0 (uncorrelated, gen) 1000
    where uncorrelated = encode testParent

--run n (x, gen) times
--    | times <= n = print n
--    | otherwise = do
--        (x', gen') <- generate (x, gen) 
--        --print $ "generation " ++ show n ++ "  - " ++ (show $ evaluateG x)
--        run (succ n) (x', gen') times


runEinFuenftel n (x, gen) times
    | times <= n = print n
    | otherwise = do 
        (x', gen') <- generate (x, gen)
        runEinFuenftel (succ n) (x', gen') times
