module UListTotal

%default total


data UList : List a -> Type where
  Empty : UList []
  Singleton : (x : a) -> List a -> UList ([x] ++ [])
  NonEmpty : (x : a) -> (xs : List a) -> UList ([x] ++ xs)


uList : (xs: List a) -> UList xs
uList [] = Empty
uList (x :: []) = Singleton x []
uList (x :: y :: xs) = case uList xs of
                              Empty => NonEmpty x (y :: [])
                              Singleton x' xs' => NonEmpty x (y :: x' :: [])
                              NonEmpty x' xs' => NonEmpty x (y :: x' :: xs')


-- uListEnd : Ord a => List a -> Maybe a
-- uListEnd xs with (uList xs)
--   uListEnd [] | Empty = Nothing
--   uListEnd ([x] ++ []) | (Singleton x []) = Just x
--   uListEnd (x :: y :: xs) | with_pat = case x > y of
--                                             False => uListEnd (x :: xs)
--                                             True => uListEnd (y :: xs)

maxTest : Ord a => List a -> Maybe a
maxTest [] = Nothing
maxTest [x] = Just x
maxTest (x :: xs) = maxTest xs


partial
maxTest2 : Ord a => List a -> Maybe a
maxTest2 [] = Nothing
maxTest2 [x] = Just x
maxTest2 (x :: y :: xs) = maxTest2 $ (if x > y then x else y) :: xs


maxTest3 : Ord a => List a -> Maybe a
maxTest3 [] = Nothing
maxTest3 [x] = Just x
maxTest3 ori@(x :: y :: xs) =
  maxTest3 $ assert_smaller ori ((if x > y then x else y) :: xs)
