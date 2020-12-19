module Grad
    ( computeGradRowLinear,
      computeGradRowLinearHelper,
      gradIntLinear,
      gradSlopeLinear,
      computeGradRowLogistic,
      hTheta,
      g
    ) where

--Compute a row of gradient
computeGradRowLinear :: [Double] -> [Double] -> [Double]
computeGradRowLinear params dataList = computeGradRowLinearHelper 0 params dataList

--Helper function to compute row of gradient
computeGradRowLinearHelper :: Int -> [Double] -> [Double] -> [Double]
computeGradRowLinearHelper n params dataList
    | n == (length dataList) = []
    | n == 0 = [(gradIntLinear params dataList)] ++ (computeGradRowLinearHelper (n+1) params dataList)
    | otherwise = [(gradSlopeLinear params dataList n)] ++ (computeGradRowLinearHelper (n+1) params dataList)

--Linear gradient function with respect to intercept
gradIntLinear :: [Double] -> [Double] -> Double
gradIntLinear params dataList = -2 * ((head dataList) - ((head params) + (sum (zipWith (*) (tail params) (tail dataList)))))

--Linear gradient function with respect to slope
gradSlopeLinear :: [Double] -> [Double] -> Int -> Double
gradSlopeLinear params dataList var = -2 *
                                    ((head dataList) - (head params) - (sum (zipWith (*) (tail params) (tail dataList)))) *
                                    (dataList !! var)

--Compute a row of the gradient in a logistic function
computeGradRowLogistic :: [Double] -> [Double] -> [Double]
computeGradRowLogistic params dataList = [h0 - y]
                                         ++  (zipWith (*) (xTail) (map (h0 -) (take (length xTail) (cycle [y]))))
                                         where h0 = hTheta params dataList
                                               xTail = tail dataList
                                               y = head dataList

--Compute loss function exponential (needed for derivatives)
hTheta :: [Double] -> [Double] -> Double
hTheta params dataList = (/) 1.0 $ 1.0 + (exp (-1 * (g params dataList)))

--Compute exponential in denominator of logistic function
g :: [Double] -> [Double] -> Double
g params dataList = sum $ zipWith (*) params ([1.0::Double] ++ (tail dataList))
