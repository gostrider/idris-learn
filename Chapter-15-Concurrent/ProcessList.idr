module ProcessList

import System.Concurrency.Channels
import ProcessLib


data ListAction : Type where
  Length : List elem -> ListAction
  Append : List elem -> List elem -> ListAction


ListType : ListAction -> Type
ListType (Length xs) = Nat
ListType (Append {elem} xs ys) = List elem


procList : Service ListType ()
procList = do
  Respond (\msg => case msg of
                     Length xs => Pure (length xs)
                     Append xs ys => Pure (xs ++ ys))
  Loop procList
