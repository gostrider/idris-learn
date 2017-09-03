module ShapeSh

public export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double


private
rectangle_area : Double -> Double -> Double
rectangle_area x y = x * y


export
area : Shape -> Double
area (Triangle x y) = 0.5 * rectangle_area x y
area (Rectangle x y) = rectangle_area x y
area (Circle x) = pi * x * x
