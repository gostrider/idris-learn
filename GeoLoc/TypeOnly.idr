module TypeOnly

import Data.Vect


%default total

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


data StoreEntry
  = CreateZone Zone
  | CreateLocation Location
  | CreateDevice Device
  | CreatePhone Phone
  | CreateUser User


data StoreRef
  = UserRef Integer
  | PhoneRef Integer
  | LocationRef Integer


Eq StoreEntry where
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


DecEq StoreEntry where
  decEq x y = case x == y of
                True => Yes storeEq
                False => No storeNotEq
    where
      storeEq : x = y
      storeEq = really_believe_me $ Refl {x}

      storeNotEq : x = y -> Void
      storeNotEq = really_believe_me $ id {a = x = y}


store : Vect 4 StoreEntry
store =
  [ CreateLocation $ MkLocation 1 "location_1"
  , CreateUser $ MkUser 1 "user_1"
  , CreatePhone $ MkPhone 1 "phone_1"
  , CreateZone $ MkZone 1 1 1
  ]


matchEntry : StoreRef -> StoreEntry -> Bool
matchEntry (LocationRef locRef) (CreateLocation l) = locRef == idx l
matchEntry (PhoneRef phoneRef)  (CreatePhone p)    = phoneRef == idx p
matchEntry (UserRef usrRef)     (CreateUser u)     = usrRef == idx u
matchEntry _                    _                  = False


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


test : StoreEntry
test = ensure (CreatePhone $ MkPhone 1 "phone_1") store


proofEntryExists : Maybe StoreEntry -> Maybe StoreEntry
proofEntryExists (Just x) =
  case isElem x store of
    (Yes prf)   => Just $ ensure x store
    (No contra) => Nothing
proofEntryExists Nothing = Nothing


exist : StoreRef -> Maybe StoreEntry
exist x = proofEntryExists $ find (matchEntry x) store


toUser : Maybe StoreEntry -> Maybe User
toUser (Just (CreateUser u)) = Just u
toUser _                     = Nothing


toLocation : Maybe StoreEntry -> Maybe Location
toLocation (Just (CreateLocation l)) = Just l
toLocation _                         = Nothing


parseParams
  : (criteria : String) ->
    (user_id : Integer) ->
    (location_id : Integer) ->
    (timestamp : Integer) ->
    Maybe Params
parseParams criteria' userID' locationID' timestamp' =
  MkParams
    <$> ensureUser (UserRef userID') <*> ensureCriteria
    <*> ensureLocation (LocationRef locationID') <*> ensureTime
  where
    ensureLocation : StoreRef -> Maybe Location
    ensureLocation = toLocation . exist

    ensureUser : StoreRef -> Maybe User
    ensureUser = toUser . exist

    ensureCriteria : Maybe Criteria
    ensureCriteria = matchCriteria criteria'

    ensureTime : Maybe Timestamp
    ensureTime = Just (ValidTimestamp timestamp')


-- %name Params params
-- matchCriteriaWith : Params -> List Device
-- matchCriteriaWith params = ?matchCriteriaWith_rhs
--
--
-- main : IO ()
