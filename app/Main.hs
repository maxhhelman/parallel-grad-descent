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
            print $ descendSteps csvData [(0::Double),(0::Double)] (1000::Int) (0.1::Double)
            print $ descendTolerance csvData [(0::Double),(0::Double)] (0.00001::Double) (0.1::Double)

--Actual gradient descent algorithm (uses magnitude of gradient as stopping condition)
descendTolerance :: [[Double]] -> [Double] -> Double -> Double -> [Double]
descendTolerance csvData guess tolerance stepSize
    | tolerance <= (0::Double) = error "tolerance must be a positive value"
    | maxVal <= tolerance = guess
    | otherwise = descendTolerance (csvData) (zipWith (-) guess (computeGrad csvData guess stepSize)) tolerance stepSize
    where
        maxVal = maximum $ map abs (computeGrad csvData guess 1)

--Actual gradient descent algorithm (uses numer of steps as stopping condition)
descendSteps :: (Ord t1, Num t1, Num t2) => [[t2]] -> [t2] -> t1 -> t2 -> [t2]
descendSteps csvData guess steps stepSize
    | steps < 0 = error "you can't take negative steps"
    | steps == 0 = guess
    | otherwise = descendSteps (csvData) (zipWith (-) guess (computeGrad csvData guess stepSize)) (steps - 1) (stepSize)

--Compute the gradient
computeGrad :: Num b => [[b]] -> [b] -> b -> [b]
computeGrad csvData params stepSize = map (* stepSize) $ specialMegaFold (fmap (computeGradRow params) csvData)

--Compute a row of gradient
computeGradRow :: Num a => [a] -> [a] -> [a]
computeGradRow params dataList = computeGradRowHelper 0 params dataList


--Helper function to compute row of gradient
computeGradRowHelper :: Num a => Int -> [a] -> [a] -> [a]
computeGradRowHelper n params dataList
    | n == (length dataList) = []
    | n == 0 = [(gradIntLSRL params dataList)] ++ (computeGradRowHelper (n+1) params dataList)
    | otherwise = [(gradSlopeLSRL params dataList n)] ++ (computeGradRowHelper (n+1) params dataList)

--Gradient function with respect to intercept
gradIntLSRL :: Num a => [a] -> [a] -> a
gradIntLSRL params dataList = -2 * ((head dataList) - ((head params) + (sum (zipWith (*) (tail params) (tail dataList)))))

--Gradient function with respect to slope
gradSlopeLSRL :: Num a => [a] -> [a] -> Int -> a
gradSlopeLSRL params dataList var = -2 *
                                    ((head dataList) - (head params) - (sum (zipWith (*) (tail params) (tail dataList)))) *
                                    (dataList !! var)

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




