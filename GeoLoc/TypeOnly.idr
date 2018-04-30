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
  uid : String


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


-- Refactor locaiton_id in Zone dependent to Location idx
-- dependent pairs?
store : Vect 3 Store
store =
  [ CreateLocation $ MkLocation 1 "location_1"
  , CreatePhone $ MkPhone 1 "phone_1"
  , CreateZone $ MkZone 1 1 1
  ]


ensureEntry : (s : a) -> (xs : Vect (S n) a) -> {auto prf : Elem s xs} -> a
ensureEntry             s (s :: xs) {prf = Here       } = s
ensureEntry {n = Z    } s (x :: []) {prf = There later} = absurd later
ensureEntry {n = (S k)} s (x :: xs) {prf = There later} = ensureEntry s xs


total
findPhone : Integer -> (stores : Vect len Store) -> Maybe Store
findPhone x store = find findPhoneInner store
  where
    findPhoneInner : Store -> Bool
    findPhoneInner (CreatePhone p) =
      if x == idx p then True else False
    findPhoneInner _ = False


test : Store
test =
  ensureEntry (CreatePhone $ MkPhone 1 "phone_1") store
--
--
-- parseParams
--   : (criteria    : String) ->
--     (user_id     : Integer) ->
--     (location_id : Integer) ->
--     (timestamp   : Integer) ->
--     Params
--
--
-- %name Params params
-- matchCriteriaWith : Params -> List Device
-- matchCriteriaWith params = ?matchCriteriaWith_rhs
--
--
-- main : IO ()
