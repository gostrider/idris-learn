module GradientDecentSimple

import Data.Vect


N : Nat
N = 10


eta : Double
eta = 0.001


X : Vect N Double
X = map cast (fromList [1..N])


Y : Vect N Double
Y = map (+ 1) X


mean : Vect n Double -> Double
mean [] = 0.0
mean {n} xs = (foldl (+) 0.0 xs) / (cast n)


-- f(x) = w * x + b
-- e = 1/2 * (f(x) - y)^2
-- de / dw = de/df df/dw = ( f(x) - y ) x
ddw : (diff : Vect N Double) -> Double
ddw diff = sum $ zipWith (*) X diff


-- de / db = de/df df/db = ( f(x) - y ) 1
ddb : (diff: Vect N Double) -> Double
ddb diff = sum diff


predict : (w : Double) -> (b: Double) -> (x : Double) -> Double
predict w b x = w * x + b


error : (y_pred : Double) -> (y_true : Double) -> Double
error y_pred y_true = 0.5 * (y_pred - y_true) * (y_pred - y_true)


optimize : (w : Double) -> (b : Double) -> (Double, Double, Double)
optimize w b = let y_pred = map (predict w b) X
                   diff = zipWith (-) y_pred Y
                   err = mean $ zipWith error y_pred Y
                   new_w = w - eta * (ddw diff)
                   new_b = b - eta * (ddb diff)
                   in (err, new_w, new_b)


loop : Double -> Double -> Double -> IO ()
loop w b eps = do
  let (err, new_w, new_b) = optimize w b
  putStrLn $ (show err) ++ " " ++ (show new_w) ++ " " ++ (show new_b)
  if err < eps
    then pure ()
    else do loop new_w new_b eps
