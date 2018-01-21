module Exercise_7_1


data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double


rectangle_area : Double -> Double -> Double
rectangle_area x y = x * y


area : Shape -> Double
area (Triangle x y) = 0.5 * rectangle_area x y
area (Rectangle x y) = rectangle_area x y
area (Circle x) = pi * x * x


-- Exercis 7.1.1
Eq Shape where
  (==) (Triangle x  y) (Triangle x'  y') = x == x' && y == y'
  (==) (Rectangle x y) (Rectangle x' y') = x == x' && y == y'
  (==) (Circle    x)   (Circle    x')    = x == x'
  (==) _               _                 = False

  x /= y = not $ x == y


-- Exercis 7.1.2
Ord Shape where
  compare x y = (area x) `compare` (area y)


Show Shape where
  show (Triangle  x y) = "Triangle "  ++ show x ++ " " ++ show y
  show (Rectangle x y) = "Rectangle " ++ show x ++ " " ++ show y
  show (Circle    x)   = "Circle "    ++ show x


testShape : List Shape
testShape = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
