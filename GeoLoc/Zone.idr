module Zone

%access public export


record Phone where
  constructor MkPhone
  phoneUUID : String
  -- ID : Integer
  -- userID : Integer


record Zone where
  constructor MkZone
  zone_id : Integer
  radius : Double
  enabled : Bool
  -- locationID : Integer
  -- phoneID : Integer


record ZoneRule where
  constructor MkRule
  zoneID : Integer
  enterTrigger : Trigger
  leaveTrigger : Trigger
  name : String
  deviceUUID : String


record ZoneActivity where
  constructor MkActivity
  zone : Zone
  enter : Bool
  phone_id : Integer
  user_id : Integer
  created_on : Integer
  updated_on : Integer
  enter_at : Integer
  leave_at : Maybe Integer


data UserZoneActivity
  = Enter ZoneActivity
  | Leave ZoneActivity
