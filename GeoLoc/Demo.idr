module Demo

import Zone
import Criteria

zone1 : Zone
zone1 = MkZone 1 2.0 True


zone2 : Zone
zone2 = MkZone 2 2.0 True


userZone1 : ZoneActivity
userZone1 = MkActivity
  zone True phone user
  create update enter leave
  where
    zone   = zone1
    phone  = 1
    user   = 1
    create = 1
    update = 1
    enter  = 1
    leave  = Nothing


userZone2 : ZoneActivity
userZone2 = MkActivity
  zone True phone user
  create update enter leave
  where
    zone   = zone2
    phone  = 2
    user   = 2
    create = 2
    update = 2
    enter  = 2
    leave  = Nothing


someEntry : List UserZoneActivity
someEntry
  = [ Enter userZone1
    , Enter userZone2
      -- Leave
    ]

{-

userID -> zoneID -> phoneUUID -> phoneID -> enter -> updated_on

program -> IO (List UserID)
program =
  matchCriteriaWith
  ("criteria" currentUser timestamp)
  ZoneActivity
  DeviceList?

-}
