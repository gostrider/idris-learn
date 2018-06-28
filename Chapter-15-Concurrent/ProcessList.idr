module ProcessList

import System.Concurrency.Channels
import ProcessLib


data ListAction : Type where
  Length : List elem -> ListAction
  Append : List elem -> List elem -> ListAction


ListType : ListAction -> Type
ListType (Length xs) = Nat
ListType (Append {elem} xs ys) = List elem


callback : (msg : ListAction) -> Process ListType (ListType msg) Ready Ready
callback (Length xs) = Pure $ length xs
callback (Append xs ys) = Pure (xs ++ ys)


total
procList : Service ListType ()
procList = do
  Respond callback
  Loop procList


procMain : Client ()
procMain = do
  Just list <- Spawn procList
    | Nothing => Action (putStrLn "Spawn failed")
  len <- Request list $ Length [1, 2, 3]
  Action $ printLn len
  app <- Request list $ Append [1, 2, 3] [4, 5, 6]
  Action $ printLn app
