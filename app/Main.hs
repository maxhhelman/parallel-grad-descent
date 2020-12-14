module Main where

-- to be created
import Grad
import System.Environment(getArgs)
import System.Exit(die)
import Data.List(isInfixOf, concat)
import Data.Array.Repa( Array, fromListUnboxed, backpermute, foldP, computeP, toList, D, U, Source, extent )
import Data.Array.Repa.Index

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

main :: IO()
main = do
            args <- getArgs
            input <- case args of
                [f, method, guess] -> return [f, method, guess]
                _ -> do
                        die $ "Usage: grad-descent <filename> <loss function: linear/logistic> <guess array>"
            csvData <- getCSVData (head input)

            let linMatch = or $ map ($ (head $ tail input)) (map isInfixOf ["Line", "Linear", "line", "linear"])
            let logMatch = or $ map ($ (head $ tail input)) (map isInfixOf ["Log", "Logistic", "log", "logistic"])

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
computeGrad csvData gradFunc params stepSize = map (* stepSize) $ specialMegaFold (fmap (gradFunc params) csvData)

--Applies a fold to each column in the dataframe
specialMegaFold :: [[Double]] -> [Double]
specialMegaFold [] = error "specialMegaFold not long enough"
specialMegaFold [x] = x
specialMegaFold xx@(x:xs:xss)
    | (length xx) == 2 = zipWith (+) x xs
    | otherwise = specialMegaFold ((zipWith (+) x xs):xss)


parallelMegaFold :: [[Double]] -> [Double]
parallelMegaFold [] = error "parallelMegaFold not long enough"
parallelMegaFold [x] = x
parallelMegaFold nested@(x:xs:xss) = parallelComputeSum nested

parallelComputeSum :: [[Double]] -> [Double]
parallelComputeSum nested@(x:xs:xss) = do
                    let x = fromListUnboxed (Z :. ((length nested)::Int) :. ((length $ (head nested))::Int) ) (concat nested)
                    let xTranspose = transpose2D x
                    let [xNDTranspose] = computeP xTranspose :: [Array U DIM2 Double]
                    let mResult = foldP (+) 0 xNDTranspose
                    result <- mResult
                    toList result


transpose2D :: (Source r e) => Array r DIM2 e -> Array D DIM2 e
transpose2D a = backpermute (swap e) swap a
     where
       e = extent a
       swap (Z :. i :. j) = Z :. j :. i


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
