module TypeOnly

import Data.Vect


data Criteria = FirstIn | LastOut | ACOn | ACOff | Always


data Timestamp = ValidTimestamp Integer | InvalidTimestamp


record Location where
  constructor MkLocation
  idx  : Integer
  name : String


record Device where
  constructor MkDevice
  idx : Integer
  uid : String


record Phone where
  constructor MkPhone
  idx : Integer
  uid : String


record User where
  constructor MkUser
  idx : Integer
  name : String


record Zone where
  constructor MkZone
  idx : Integer
  phone_id : Integer
  location_id : Integer


record Params where
  constructor MkParams
  user : User
  criteria : Criteria
  location : Location
  timestamp : Timestamp


data Store
  = CreateZone Zone
  | CreateLocation Location
  | CreateDevice Device
  | CreatePhone Phone
  | CreateUser User


store : Vect 4 Store
store =
  [ CreateLocation $ MkLocation 1 "location_1"
  , CreateUser $ MkUser 1 "user_1"
  , CreatePhone $ MkPhone 1 "phone_1"
  , CreateZone $ MkZone 1 1 1
  ]


data StoreRef
  = UserRef Integer
  | PhoneRef Integer
  | LocationRef Integer


matchField : StoreRef -> Store -> Bool
matchField (LocationRef locRef) (CreateLocation l) = locRef == idx l
matchField (PhoneRef phoneRef)  (CreatePhone p)    = phoneRef == idx p
matchField (UserRef usrRef)     (CreateUser u)     = usrRef == idx u
matchField _                    _                  = False


matchCriteria : String -> Maybe Criteria
matchCriteria "first_in" = Just FirstIn
matchCriteria "last_out" = Just LastOut
matchCriteria "ac_on"    = Just ACOn
matchCriteria "ac_off"   = Just ACOff
matchCriteria "always"   = Just Always
matchCriteria _          = Nothing


ensure : (s : a) -> (xs : Vect (S n) a) -> {auto prf : Elem s xs} -> a
ensure             s (s :: xs) {prf = Here       } = s
ensure {n = Z    } s (x :: []) {prf = There later} = absurd later
ensure {n = (S k)} s (x :: xs) {prf = There later} = ensure s xs


-- test : Store
-- test = ensure (CreatePhone $ MkPhone 1 "phone_1") store


Eq Store where
  (CreateZone (MkZone x1 x2 x3)) == (CreateZone (MkZone y1 y2 y3)) =
    x1 == y1 && x2 == y2 && x3 == y3

  (CreateLocation (MkLocation x1 x2)) == (CreateLocation (MkLocation y1 y2)) =
    x1 == y1 && x2 == y2

  (CreateDevice (MkDevice x1 x2)) == (CreateDevice (MkDevice y1 y2)) =
    x1 == y1 && x2 == y2

  (CreatePhone (MkPhone x1 x2)) == (CreatePhone (MkPhone y1 y2)) =
    x1 == y1 && x2 == y2

  (CreateUser (MkUser x1 x2)) == (CreateUser (MkUser y1 y2)) =
    x1 == y1 && x2 == y2

  _ == _ = False


DecEq Store where
  decEq x y = case x == y of
                True => Yes storeEq
                False => No storeNotEq
    where
      storeEq : x = y
      storeEq = really_believe_me $ Refl {x}

      storeNotEq : x = y -> Void
      storeNotEq = really_believe_me $ id {a = x = y}


testEntryExists : Maybe Store -> Maybe Store
testEntryExists (Just x) =
  case isElem x store of
    (Yes prf)   => Just $ ensure x store
    (No contra) => Nothing
testEntryExists Nothing = Nothing


toUser : Maybe Store -> Maybe User
toUser (Just (CreateUser u)) = Just u
toUser _                     = Nothing


toLocation : Maybe Store -> Maybe Location
toLocation (Just (CreateLocation l)) = Just l
toLocation _                         = Nothing


parseParams
  : (criteria    : String) ->
    (user_id     : Integer) ->
    (location_id : Integer) ->
    (timestamp   : Integer) ->
    Maybe Params
parseParams criteria' userID' locationID' timestamp' =
  MkParams
    <$> toUser ensureUser         <*> ensureCriteria
    <*> toLocation ensureLocation <*> ensureTime
  where
    ensureLocation : Maybe Store
    ensureLocation =
      testEntryExists $ find (matchField $ LocationRef locationID') store

    ensureUser : Maybe Store
    ensureUser =
      testEntryExists $ find (matchField $ UserRef userID') store

    ensureCriteria : Maybe Criteria
    ensureCriteria = matchCriteria criteria'

    ensureTime : Maybe Timestamp
    ensureTime = Just $ ValidTimestamp timestamp'


-- %name Params params
-- matchCriteriaWith : Params -> List Device
-- matchCriteriaWith params = ?matchCriteriaWith_rhs
--
--
-- main : IO ()
