module GradientDecent

import Data.Vect

houseSizeSample : List Double
houseSizeSample =
    [ 1100, 1400, 1425, 1550, 1600
    , 1700, 1700, 1875, 2350, 3450
    ]

housePriceSample : List Double
housePriceSample =
    [ 199000, 245000, 319000, 240000, 312000
    , 279000, 310000, 308000, 405000, 324000
    ]

applyA2 : (Double -> Double -> Double) -> List Double -> List Double -> List Double
applyA2 _ [] yps = []
applyA2 _ ys []  = []
applyA2 f (y :: ys) (yp :: yps) =
    f y yp :: applyA2 f ys yps


minMaxNormalise : Double -> Double -> Double -> Double
minMaxNormalise min' max' n =
    (n - min') / (max' - min')


normalise : List Double -> List Double
normalise axis =
  let Just min' = head' $ sort axis
      Just max' = last' $ sort axis
  in map (minMaxNormalise min' max') axis


yPrediction : Double -> Double -> Double -> Double
yPrediction a b price = a + b * price


squaredError : Double -> Double -> Double
squaredError y yp =
    (y - yp) `pow` 2 * 0.5


errorFunc : List Double -> List Double -> Double
errorFunc y yp =
    sum $ applyA2 squaredError y yp


updateWeigth : Double -> Double -> Double -> Double -> Double
updateWeigth weight rate dsse dx =
    weight - rate * (dsse / dx)

normalizedSize : List Double
normalizedSize = normalise houseSizeSample


normalizedPrice : List Double
normalizedPrice = normalise housePriceSample


gradient : Double -> Double -> Double -> (Double, Double, Double)
gradient weightA weightB learningRate =
  let
    pricePred = map (yPrediction weightA weightB) normalizedPrice
    err = errorFunc normalizedPrice pricePred

    -- dA y yp = -(y - yp)
    -- dB x dA = dA * x

    -- derivativeOfA = applyA2 dA normalizedPrice pricePred
    -- derivativeOfB = applyA2 dB normalizedSize derivativeOfA

    -- a' = updateWeigth weightA learningRate err (sum derivativeOfA)
    -- b' = updateWeigth weightB learningRate err (sum derivativeOfB)
  in
    (err, weightA, weightB)

-- optmise 214 0.45 0.75 0
optmise : Int -> Double -> Double -> Double -> Double -> IO ()
optmise 0 _ _ err state = do
    print ("Final: " ++ show err)
optmise count weightA weightB err state = do
    let (e, a', b') = gradient weightA weightB 0.01
    -- print ("step " ++ show count ++ ", error " ++ show e)
    if e < state
    then optmise (count - 1) a' b' e e
    else optmise 0 a' b' e e
