module GeoState


data ZoneState = Empty Nat
               | Occupied Nat


data ZoneCmd : Type -> ZoneState -> ZoneState -> Type where
  FirstIn : ZoneCmd () (Empty Z)        (Occupied (S Z))
  LastOut : ZoneCmd () (Occupied (S Z)) (Empty Z)
  Enter   : ZoneCmd () (Occupied k)     (Occupied (S k))
  Leave   : ZoneCmd () (Occupied (S k)) (Occupied k)

  Display : String -> ZoneCmd () state state
  GetCount : ZoneCmd Integer state state
  Pure  : ty -> ZoneCmd ty state state
  (>>=) : ZoneCmd a st1 st2 ->
          (a -> ZoneCmd b st2 st3) -> ZoneCmd b st1 st3


getCount : (zone : ZoneState) -> Integer
getCount (Empty k) = 0
getCount (Occupied k) = toIntegerNat k


runEntry : (zone : ZoneState) -> ZoneCmd ty inState outState -> (ty, ZoneState)
runEntry _ FirstIn              = ((), Occupied (S Z))
runEntry _ LastOut              = ((), Empty Z)
runEntry (Occupied k) Enter     = ((), Occupied (S k))
runEntry (Occupied (S k)) Leave = ((), Occupied k)
runEntry zone (Display x)       = ((), zone)
runEntry zone GetCount          = (getCount zone, zone)
runEntry zone (Pure x)          = (x, zone)
runEntry zone (x >>= cmd)       = let (cmdRes, zone') = runEntry zone x
                                  in runEntry zone' $ cmd cmdRes


testFirstIn : ZoneCmd () (Empty Z) (Occupied (S Z))
testFirstIn = FirstIn


testCountEntry : ZoneCmd Integer (Empty 0) (Occupied 2)
testCountEntry = do
  FirstIn
  Enter
  c1 <- GetCount
  Pure c1


testSomeIO : ZoneCmd () (Empty Z) (Empty Z)
testSomeIO = do
  FirstIn
  Display "1 entry"
  LastOut
