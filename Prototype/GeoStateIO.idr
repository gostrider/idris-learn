module GeoStateIO


data ZoneState = Empty Nat
               | Occupied Nat


data ZoneCmd : Type -> ZoneState -> ZoneState -> Type where
  FirstIn : ZoneCmd () (Empty Z)        (Occupied (S Z))
  LastOut : ZoneCmd () (Occupied (S Z)) (Empty Z)
  Enter   : ZoneCmd () (Occupied k)     (Occupied (S k))
  Leave   : ZoneCmd () (Occupied (S k)) (Occupied k)

  Display  : String -> ZoneCmd () state state
  GetCount : ZoneCmd Integer state state

  Pure  : ty -> ZoneCmd ty state state
  (>>=) : ZoneCmd a st1 st2 -> (a -> ZoneCmd b st2 st3) -> ZoneCmd b st1 st3


getCount : (zone : ZoneState) -> Integer
getCount (Empty k) = 0
getCount (Occupied k) = toIntegerNat k


runEntry : (zone : ZoneState) -> ZoneCmd ty inState outState -> IO (ty, ZoneState)
runEntry _ FirstIn              = pure ((), Occupied (S Z))
runEntry _ LastOut              = pure ((), Empty Z)
runEntry (Occupied k) Enter     = pure ((), Occupied (S k))
runEntry (Occupied (S k)) Leave = pure ((), Occupied k)
runEntry zone (Display x)       = putStrLn x >>= \_ => pure ((), zone)
runEntry zone GetCount          = pure (getCount zone, zone)
runEntry zone (Pure x)          = pure (x, zone)
runEntry zone (x >>= cmd)       = do (cmdRes, zone') <- runEntry zone x
                                     runEntry zone' $ cmd cmdRes


testFirstIn : ZoneCmd () (Empty Z) (Occupied (S Z))
testFirstIn = FirstIn


countEntry : ZoneCmd Integer (Empty 0) (Occupied 2)
countEntry = do
  FirstIn
  Enter
  c1 <- GetCount
  Pure c1


testSomeIO : ZoneCmd () (Empty Z) (Empty Z)
testSomeIO = do
  FirstIn
  c1 <- GetCount
  Display $ (cast c1) ++ " in zone"
  LastOut


main : IO ()
main = runEntry (Empty 0) testSomeIO >>= \_ => return ()
-- do runEntry (Empty 0) testSomeIO
--    return ()
