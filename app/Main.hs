module Main where

import Grad
import System.Environment(getArgs)
import System.Exit(die)
import Data.List(isInfixOf)

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
                [f, method, guess, parseq] -> return [f, method, guess, parseq]
                _ -> do
                        die $ "Usage: grad-descent <filename> <loss function: linear/logistic> <guess array> <parallel/sequential>"
            csvData <- getCSVData (head input)

            let linMatch = or $ map ($ (head $ tail input)) (map isInfixOf ["Linear", "linear", "LINEAR"])
            let logMatch = or $ map ($ (head $ tail input)) (map isInfixOf ["Logistic", "logistic", "LOGISTIC"])

            appLoss <- case (linMatch || logMatch) of
                 True -> if linMatch then (return computeGradRowLinear) else (return computeGradRowLogistic)
                 False -> do
                            die $ "Choose either Linear or Logistic loss functions"

            let guess = read (head $ tail $ tail input) :: [Double]
            let choice = last input

--            print $ descendTolerance csvData computeGsadRowLogistic [5.1,0.1,1.1] (0.00001) (0.001::Double)
            print $ descendSteps choice csvData appLoss guess (1000::Int) (0.0000001::Double)
--            print $ descendTolerance csvData computeGradRowLogistic [0.0,0.0] (0.001) (0.0001)
--            print $ descendSteps csvData computeGradRowLogistic [0.0,0.0] (10000::Int) (0.001::Double)

--If we add more loss functions later, we could have this structure
computeGradDecider :: Bool -> Bool -> ([Double] -> [Double] -> [Double])
computeGradDecider linOutcome logOutcome
    | linOutcome = computeGradRowLinear
    | logOutcome = computeGradRowLogistic
    | otherwise = computeGradRowLogistic


