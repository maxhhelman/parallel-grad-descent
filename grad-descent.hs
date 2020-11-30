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
            sequence_ $ fmap (putStrLn . show) (computeGrad (csvData) [(+1.0),(+2.0)])

--Creates the 'dataframe' structure - feel free to change
getCSVData :: FilePath -> IO [[Double]]
getCSVData filename = do
                        lns <- fmap lines (readFile filename)
                        return $ map (map (\x -> read x::Double)) (map words (map rep (tail lns)))

--Compute the gradient
computeGrad :: Num c => [[t]] -> [t -> c] -> [c]
computeGrad csvData gradFList = specialMegaFold $ fmap (applyList gradFList) csvData

--Preprocessing for CSV files (turns all commas into spaces so we can use words)
rep :: [Char] -> [Char]
rep [] = []
rep (x:xs)
    | x == ',' = [' '] ++ (rep xs)
    | otherwise = [x] ++ (rep xs)

--Apply a list of functions to a list of values
applyList :: [t -> a] -> [t] -> [a]
applyList [] [] = []
applyList [] (_:_) = error "dimension error with gradient and data (length of gradient < length of data)"
applyList (_:_) [] = error "dimension error with gradient and data (length of gradient > length of data)"
applyList (f:fs) (x:xs) = [f x] ++ (applyList fs xs)

--Applies a fold to each column in the dataframe
specialMegaFold :: Num c => [[c]] -> [c]
specialMegaFold [] = error "specialMegaFold not long enough"
specialMegaFold [_] = error "specialMegaFold not long enough"
specialMegaFold xx@(x:xs:xss)
    | (length xx) == 2 = zipWith (+) x xs
    | otherwise = specialMegaFold ((zipWith (+) x xs):xss)