module ShapePlus


data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double


data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture


maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe x Nothing = x
maxMaybe Nothing y = y
maxMaybe (Just x) (Just y) = case compare x y of
                               LT => Just y
                               EQ => Just x
                               GT => Just x


area : Shape -> Double
area (Triangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x


pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic


biggestTriangle : Picture -> Maybe Double
biggestTriangle (Rotate _ pic)             = biggestTriangle pic
biggestTriangle (Translate _ _ pic)        = biggestTriangle pic
biggestTriangle (Combine pic pic1)         = maxMaybe (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Primitive (Triangle x y)) = Just $ pictureArea $ Primitive $ Triangle x y
biggestTriangle (Primitive _)              = Nothing


testPic1 : Picture
testPic1 = Combine (Primitive $ Triangle 2 3)
                   (Primitive $ Triangle 2 4)


testPic2 : Picture
testPic2 = Combine (Primitive $ Rectangle 1 3)
                   (Primitive $ Circle 4)


testPic3 : Picture
testPic3 = Combine (Translate 5 5 $ Primitive $ Rectangle 20 10)
                   (Combine (Translate 35 5 $ Primitive $ Circle 5)
                            (Translate 15 25 $ Primitive $ Triangle 10 10))
