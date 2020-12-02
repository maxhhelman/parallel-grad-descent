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
            print $ descend csvData [(0.9::Double),(0.1::Double)] (5::Int) (0.01::Double)
            --sequence_ $ fmap (putStrLn . show) (computeGrad (csvData) [0,0])

--Actual gradient descent algorithm
descend :: (Ord t1, Num t1, Num t2) => [[t2]] -> [t2] -> t1 -> t2 -> [t2]
descend csvData guess steps stepSize
    | steps < 0 = error "you can't take negative steps"
    | steps == 0 = guess
    | otherwise = descend (csvData) (zipWith (+) guess (computeGrad csvData guess stepSize)) (steps - 1) (stepSize)

--Compute the gradient
computeGrad :: Num b => [[b]] -> [b] -> b -> [b]
computeGrad csvData params stepSize = fmap (* stepSize) $ specialMegaFold (fmap (computeGradRow (head params) (last params)) csvData)

--Compute a row of gradient (single variable)
computeGradRow :: Num a => a -> a -> [a] -> [a]
computeGradRow beta_0 beta_1 dataList = [(gradInt beta_0 beta_1 dataList),(gradSlope beta_0 beta_1 dataList)]

--Gradient function with respect to intercept (single variable)
gradInt :: Num a => a -> a -> [a] -> a
gradInt beta0 beta1 dataList = -2 * ((head dataList) - beta0 - (beta1 * (last dataList)))

--Gradient function with respect to slope (single variable)
gradSlope :: Num a => a -> a -> [a] -> a
gradSlope beta0 beta1 dataList = -2 * ((head dataList) - beta0 - (beta1 * (last dataList))) * (-1 * (last dataList))

--Creates the 'dataframe' structure - feel free to change
getCSVData :: FilePath -> IO [[Double]]
getCSVData filename = do
                        lns <- fmap lines (readFile filename)
                        return $ map (map (\x -> read x::Double)) (map words (map rep (tail lns)))

--Applies a fold to each column in the dataframe
specialMegaFold :: Num c => [[c]] -> [c]
specialMegaFold [] = error "specialMegaFold not long enough"
specialMegaFold [_] = error "specialMegaFold not long enough"
specialMegaFold xx@(x:xs:xss)
    | (length xx) == 2 = zipWith (+) x xs
    | otherwise = specialMegaFold ((zipWith (+) x xs):xss)

--Preprocessing for CSV files (turns all commas into spaces so we can use words)
rep :: [Char] -> [Char]
rep [] = []
rep (x:xs)
    | x == ',' = [' '] ++ (rep xs)
    | otherwise = [x] ++ (rep xs)