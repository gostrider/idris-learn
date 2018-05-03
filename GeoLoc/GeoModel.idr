module GeoModel


record User where
  constructor MkUser
  idx : Integer
  name : String


record Phone where
  constructor MkPhone
  idx : Integer
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
  created_on : Integer
  updated_on : Integer
  enter_at : Integer
  leave_at : Integer


record Trigger where
  constructor MkTrigger
  idx : Integer
  user : User
  device : Device
  name : String
  action : String
  trigger_rule : String


record Rule where
  constructor MkRule
  idx : Integer
  name : String
  enter_trigger : Trigger
  leave_trigger : Trigger
  deleted_on : Integer


record Zone where
  constructor MkZone
  user : User
  -- Probably multiple phone?
  phone_id : Integer
  location : Location
  latest_activity : UserActivity
  Rules : List Rule
