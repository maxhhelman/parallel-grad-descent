module Main where

-- to be created
import Lib
import System.Environment(getArgs)
import System.Exit(die)

{- |
Module      :  <File name or $Header$ to be replaced automatically>
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

main :: IO ()
main = do
            args <- getArgs
            filename <- case args of
                [f] -> return (f)
                _ -> do
                        die $ "Usage: grad-descent <filename>"
            csvData <- getCSVData filename
            print $ descendSteps csvData computeGradRowLinear [(5.1::Double),(0.1::Double),(1.1::Double)] (10000::Int) (0.001::Double)
            print $ descendTolerance csvData computeGradRowLinear [(0.2::Double),(0.2::Double),(1.0::Double)] (0.00001::Double) (0.001::Double)
            print $ descendSteps csvData computeGradRowLogistic [(0.0::Double),(0.0::Double)] (10000::Int) (0.001::Double)

--Actual gradient descent algorithm (uses magnitude of gradient as stopping condition)
descendTolerance :: [a] -> ([Double] -> a -> [Double]) -> [Double] -> Double -> Double -> [Double]
descendTolerance csvData gradFunc guess tolerance stepSize
    | tolerance < (0::Double) = error "tolerance must be a positive value"
    | maxVal <= tolerance = guess
    | otherwise = descendTolerance (csvData) gradFunc (zipWith (-) guess (computeGrad csvData gradFunc guess stepSize)) tolerance stepSize
    where
        maxVal = maximum $ map abs (computeGrad csvData gradFunc guess stepSize)

--Actual gradient descent algorithm (uses numer of steps as stopping condition)
descendSteps :: (Ord t1, Num t1, Num t2) => [a] -> ([t2] -> a -> [t2]) -> [t2] -> t1 -> t2 -> [t2]
descendSteps csvData gradFunc guess steps stepSize
    | steps < 0 = error "you can't take negative steps"
    | steps == 0 = guess
    | otherwise = descendSteps (csvData) gradFunc (zipWith (-) guess (computeGrad csvData gradFunc guess stepSize)) (steps - 1) (stepSize)

--Compute the gradient
computeGrad :: Num b => [a] -> (t -> a -> [b]) -> t -> b -> [b]
computeGrad csvData gradFunc params stepSize = map (* stepSize) $ specialMegaFold (fmap (gradFunc params) csvData)

--Applies a fold to each column in the dataframe
specialMegaFold :: Num c => [[c]] -> [c]
specialMegaFold [] = error "specialMegaFold not long enough"
specialMegaFold [x] = x
specialMegaFold xx@(x:xs:xss)
    | (length xx) == 2 = zipWith (+) x xs
    | otherwise = specialMegaFold ((zipWith (+) x xs):xss)

--Creates the 'dataframe' structure - feel free to change (O(n) lookup time is a problem)
getCSVData :: FilePath -> IO [[Double]]
getCSVData filename = do
                        lns <- fmap lines (readFile filename)
                        return $ map (map (\x -> read x::Double)) (map words (map rep (tail lns)))

--Preprocessing for CSV files (turns all commas into spaces so we can use words)
rep :: [Char] -> [Char]
rep [] = []
rep (x:xs)
    | x == ',' = [' '] ++ (rep xs)
    | otherwise = [x] ++ (rep xs)