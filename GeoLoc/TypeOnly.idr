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


data ValidEntry : Store -> Vect len Store -> Type where
  ValidPhone : ValidEntry (CreatePhone p) ((CreatePhone p) :: ss)


notExistsPhone : (x : Integer) -> Elem (CreatePhone p) [] -> Void


findPhone : Integer -> (stores : Vect len Store) -> Dec (ValidEntry (CreatePhone p) stores)
findPhone x [] = No ?findPhone_rhs_1
findPhone x ((CreatePhone p) :: xs) = case decEq x (idx p) of
                                           (Yes prf) => ?next_1
                                           (No contra) => ?next_2


%name CreateZone zone
addToStore : (elem : Store) -> (stores : Vect len Store) -> Vect (S len) Store
addToStore (CreateZone zone) stores = ?addToStore_rhs_1
  where
    ensurePhone : Dec (Elem ?phone_record stores)
addToStore elem stores = elem :: stores



parseParams
  : (criteria    : String) ->
    (user_id     : Integer) ->
    (location_id : Integer) ->
    (timestamp   : Integer) ->
    Params


%name Params params
matchCriteriaWith : Params -> List Device
matchCriteriaWith params = ?matchCriteriaWith_rhs


main : IO ()
