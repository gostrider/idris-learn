module GeoModel

import Data.Vect


%default total


data Operator = OR | AND

data Criteria = FirstIn | LastOut | ACOn | ACOff | Always

data Timestamp = ValidTimestamp Integer | InvalidTimestamp


record User where
  constructor MkUser
  name : String


record Phone where
  constructor MkPhone
  uid : String


record Device where
  constructor MkDevice
  real_device_id : String
  room_name : String


record Location where
  constructor MkLocation
  location_id : String
  name : String


record UserActivity where
  constructor MkActivity
  phone : Phone
  created_on : Integer
  updated_on : Integer
  enter_at : Integer
  leave_at : Integer


record TriggerRule where
  constructor MkTriggerRule
  criteria : Criteria
  operator : Operator


record TriggerAction where
  constructor MkTriggerAction
  mode : String
  target : Integer


record Trigger where
  constructor MkTrigger
  name : String
  action : TriggerAction
  trigger_rule : TriggerRule


record Rule where
  constructor MkRule
  name : String
  devices : List Device
  enter_trigger : Trigger
  leave_trigger : Trigger
  deleted_on : Maybe Integer


record Zone where
  constructor MkZone
  user : User
  location : Location
  rules : List Rule
  latest_activity : List UserActivity


record Params where
  constructor MkParams
  user : User
  criteria : Criteria
  location : Location
  timestamp : Timestamp


record Store (entry : Zone) where
  constructor MkStore
  size : Nat
  items : Vect size Zone


matchCriteria : String -> Maybe Criteria
matchCriteria "first_in" = Just FirstIn
matchCriteria "last_out" = Just LastOut
matchCriteria "ac_on"    = Just ACOn
matchCriteria "ac_off"   = Just ACOff
matchCriteria "always"   = Just Always
matchCriteria _          = Nothing


-- insertToStore : (entry : a) -> Store zone

createTrigger : String -> TriggerAction -> TriggerRule -> Trigger
createTrigger name action rule =
  MkTrigger name action rule


createRule : String -> Device -> Trigger -> Trigger -> Rule
createRule name device enter leave =
  MkRule name [device] enter leave Nothing


createZone : Maybe Zone
createZone =
  Just $ MkZone (MkUser "user1") (MkLocation "0001" "location1") [rule] []
  where
    enterTrigger : Trigger
    enterTrigger =
      createTrigger "enter" (MkTriggerAction "climate" (-1)) (MkTriggerRule FirstIn OR)

    leaveTrigger : Trigger
    leaveTrigger =
      createTrigger "leave" (MkTriggerAction "Off" (-1)) (MkTriggerRule LastOut OR)

    rule: Rule
    rule = createRule "FILO" (MkDevice "0001" "prototype") enterTrigger leaveTrigger


parseParams : String -> Integer -> Integer -> Integer -> Maybe Params


main : IO ()


{-
  Insert User
  CreateZone by finding existing User
-}
