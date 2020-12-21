module Main where

import Grad
import System.Environment(getArgs)
import System.Exit(die)
import Data.List(isInfixOf)
import Data.List.Split
import Control.Parallel.Strategies

{- |
Module      :  <File name or $Header$ to be replaced automatically>
Description :  Parallelized Gradient Descent algorithm for linear regression
Copyright   :  (c) <Max Helman, Riya Chakraborty>
License     :  BSD 3-Clause

Maintainer  :  mhh2148@columbia.edu, rc3242@columbia.edu
Stability   :  stable
Portability :  portable
-}

main :: IO()
main = do
            args <- getArgs
            input <- case args of
                [f, method, guess] -> return [f, method, guess]
                _ -> do
                        die $ "Usage: grad-descent <filename> <loss function: linear/logistic> <guess array>"
            csvData <- getCSVData (head input)

            let linMatch = or $ map ($ (head $ tail input)) (map isInfixOf ["Linear", "linear", "LINEAR"])
            let logMatch = or $ map ($ (head $ tail input)) (map isInfixOf ["Logistic", "logistic", "LOGISTIC"])

            appLoss <- case (linMatch || logMatch) of
                 True -> if linMatch then (return computeGradRowLinear) else (return computeGradRowLogistic)
                 False -> do
                            die $ "Choose either Linear or Logistic loss functions"

            let guess = read (head $ tail $ tail input) :: [Double]

--            print $ descendTolerance csvData computeGradRowLogistic [5.1,0.1,1.1] (0.00001) (0.001::Double)
            print $ descendSteps csvData appLoss guess (10000::Int) (0.0000000001::Double)
--            print $ descendTolerance csvData computeGradRowLogistic [0.0,0.0] (0.001) (0.0001)
--            print $ descendSteps csvData computeGradRowLogistic [0.0,0.0] (10000::Int) (0.001::Double)

--If we add more loss functions later, we could have this structure
computeGradDecider :: Bool -> Bool -> ([Double] -> [Double] -> [Double])
computeGradDecider linOutcome logOutcome
    | linOutcome = computeGradRowLinear
    | logOutcome = computeGradRowLogistic
    | otherwise = computeGradRowLogistic

--Actual gradient descent algorithm (uses magnitude of gradient as stopping condition)
descendTolerance :: [a] -> ([Double] -> a -> [Double]) -> [Double] -> Double -> Double -> [Double]
descendTolerance csvData gradFunc guess tolerance stepSize
    | tolerance < (0::Double) = error "tolerance must be a positive value"
    | maxVal <= tolerance = guess
    | otherwise = descendTolerance (csvData) gradFunc (zipWith (-) guess (computeGrad csvData gradFunc guess stepSize)) tolerance stepSize
    where
        maxVal = maximum $ map abs (computeGrad csvData gradFunc guess stepSize)

--Actual gradient descent algorithm (uses numer of steps as stopping condition)
descendSteps :: [a] -> ([Double] -> a -> [Double]) -> [Double] -> Int -> Double -> [Double]
descendSteps csvData gradFunc guess steps stepSize
    | steps < 0 = error "you can't take negative steps"
    | steps == 0 = guess
    | otherwise = descendSteps (csvData) gradFunc (zipWith (-) guess (computeGrad csvData gradFunc guess stepSize)) (steps - 1) (stepSize)

--Compute the gradient
computeGrad :: [a] -> ([Double] -> a -> [Double]) -> [Double] -> Double -> [Double]
computeGrad csvData gradFunc params stepSize = map (* stepSize) (parallelMegaFold2 (map (gradFunc params) csvData))

--Applies a fold to each column in the dataframe
specialMegaFold :: [[Double]] -> [Double]
specialMegaFold [] = []
specialMegaFold [x] = x
specialMegaFold xx@(x:xs:xss)
    | (length xx) == 2 = zipWith (+) x xs
    | otherwise = specialMegaFold ((zipWith (+) x xs):xss)

parallelMegaFold [] = return []
parallelMegaFold [x] = return x   
parallelMegaFold (x:xs:[]) = return $ zipWith (+) x xs
parallelMegaFold (x:xs:xss) = do
                                x' <- rpar $ zipWith (+) x xs
                                xs' <- parallelMegaFold xss
                                return $ zipWith (+) x' xs'

parallelMegaFold2 [] = []
parallelMegaFold2 [x] = x   
parallelMegaFold2 (x:xs:[]) = zipWith (+) x xs
parallelMegaFold2 xx@(x:xs:xss) = 
                                    if length xx == 1 then
                                        head xx
                                    else parallelMegaFold2 $ parMap (rseq) specialMegaFold chunks
                                    where
                                        chunks = chunksOf ((length xx) `div` 2) xx

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
