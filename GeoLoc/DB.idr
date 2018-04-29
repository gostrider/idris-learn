module DB

import Zone

%access export


user1 : User
user1 = MkUser 1 "user_1"


user2 : User
user2 = MkUser 2 "user_2"


phone1 : Phone
phone1
  = MkPhone 1 "phone_1" (ValidUserID 1)


phone2 : Phone
phone2 = MkPhone 2 "phone_2" (ValidUserID 2)


zone1 : Zone
zone1 = MkZone 1 (ValidPhoneID 1) "location_1"


zone2 : Zone
zone2 = MkZone 2 (ValidPhoneID 2) "location_2"


mutual
  enterTrigger : Trigger
  enterTrigger = MkTrigger "Temperature" "first_in"

  leaveTrigger : Trigger
  leaveTrigger = MkTrigger "Off" "last_out"

  rule1 : ZoneRule
  rule1 = MkRule (ValidZoneID 1) (ValidDeviceID "device_1") enterTrigger leaveTrigger

  rule2 : ZoneRule
  rule2 = MkRule (ValidZoneID 2) (ValidDeviceID "device_2") enterTrigger leaveTrigger


userZone1 : ZoneActivity
userZone1 =
  MkActivity
    (ValidZoneID 1)
    True
    (ValidPhoneID 1)
    (ValidUserID 1)
    (ValidTimestamp 1)
    (ValidTimestamp 1)
    (ValidTimestamp 1)
    Nothing


userZone2 : ZoneActivity
userZone2 =
  MkActivity
    (ValidZoneID 2)
    True
    (ValidPhoneID 2)
    (ValidUserID 2)
    (ValidTimestamp 2)
    (ValidTimestamp 2)
    (ValidTimestamp 2)
    Nothing


someEntry : List ZoneActivity
someEntry
  = [ userZone1
    , userZone2
    ]


users : List User
users
  = [ user1
    , user2
    ]


zones : List Zone
zones
  = [ zone1
    , zone2
    ]
