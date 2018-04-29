module Zone

%access public export


data DeviceID
  = ValidDeviceID String
  | InvalidDeviceID


data UserID
  = ValidUserID Integer
  | InvalidUserID


data PhoneID
  = ValidPhoneID Integer
  | InvalidPhoneID


data ZoneID
  = ValidZoneID Integer
  | InvalidZoneID


data Timestamp
  = ValidTimestamp Integer
  | InvalidTimestamp


Eq UserID where
  (ValidUserID x) == (ValidUserID y) = x == y
  _ == _ = False


Eq Timestamp where
  (ValidTimestamp x) == (ValidTimestamp y) = x == y
  _ == _ = False


Ord Timestamp where
  compare (ValidTimestamp x) (ValidTimestamp y) =
    compare x y


record User where
  constructor MkUser
  idx : Integer
  uid : String


record Phone where
  constructor MkPhone
  idx : Integer
  uid : String
  user_id : UserID


record Zone where
  constructor MkZone
  idx : Integer
  phone_id : PhoneID
  location_id : String


record Trigger where
  constructor MkTrigger
  action_mode : String
  criteria : String


record ZoneRule where
  constructor MkRule
  zone_id       : ZoneID
  device_id     : DeviceID
  enter_trigger : Trigger
  leave_trigger : Trigger


record ZoneActivity where
  constructor MkActivity
  zone_id : ZoneID
  enter : Bool
  phone_id : PhoneID
  user_id : UserID
  created_on : Timestamp
  updated_on : Timestamp
  enter_at : Timestamp
  leave_at : Maybe Timestamp


-- data UserZoneActivity
--   = Enter ZoneActivity
--   | Leave ZoneActivity
