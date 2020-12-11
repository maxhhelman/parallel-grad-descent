module Lib
    ( computeGradRowLinear,
      computeGradRowLinearHelper,
      gradIntLinear,
      gradSlopeLinear,
      computeGradRowLogistic,
      hTheta,
      g
    ) where

--Compute a row of gradient
computeGradRowLinear :: Num a => [a] -> [a] -> [a]
computeGradRowLinear params dataList = computeGradRowLinearHelper 0 params dataList

--Helper function to compute row of gradient
computeGradRowLinearHelper :: Num a => Int -> [a] -> [a] -> [a]
computeGradRowLinearHelper n params dataList
    | n == (length dataList) = []
    | n == 0 = [(gradIntLinear params dataList)] ++ (computeGradRowLinearHelper (n+1) params dataList)
    | otherwise = [(gradSlopeLinear params dataList n)] ++ (computeGradRowLinearHelper (n+1) params dataList)

--Linear gradient function with respect to intercept
gradIntLinear :: Num a => [a] -> [a] -> a
gradIntLinear params dataList = -2 * ((head dataList) - ((head params) + (sum (zipWith (*) (tail params) (tail dataList)))))

--Linear gradient function with respect to slope
gradSlopeLinear :: Num a => [a] -> [a] -> Int -> a
gradSlopeLinear params dataList var = -2 *
                                    ((head dataList) - (head params) - (sum (zipWith (*) (tail params) (tail dataList)))) *
                                    (dataList !! var)

--Compute a row of the gradient in a logistic function
computeGradRowLogistic :: [Double] -> [Double] -> [Double]
computeGradRowLogistic params dataList = [(hTheta params dataList) - (head dataList)] 
                                        ++ (tail (zipWith (*) dataList (map (subtract (head dataList)) 
                                        (take (length dataList) (cycle [hTheta params dataList])))))

--Compute loss function exponential (needed for derivatives)
hTheta :: [Double] -> [Double] -> Double
hTheta params dataList = (/) 1.0 $ 1.0 + (exp (-1 * (g params dataList)))

--Compute exponential in denominator of logistic function
g :: [Double] -> [Double] -> Double
g params dataList = sum $ zipWith (*) params ([1.0::Double] ++ (tail dataList))
