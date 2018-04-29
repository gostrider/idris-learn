module Criteria

import Zone
import DB


public export
data Criteria = FirstIn
              | LastOut
              | ACOn
              | ACOff
              | Always


userInZone : UserID -> Timestamp -> ZoneActivity -> Bool
userInZone user currentTime userZone =
  entered && notCurrUser && enterOnce
  where
    entered     = enter userZone
    notCurrUser = user_id userZone /= user
    enterOnce   = currentTime > enter_at userZone


ordinalEntry : UserID -> Timestamp -> List ZoneActivity
ordinalEntry user currentTime =
  filter (userInZone user currentTime) DB.someEntry


applianceState : String -> List ZoneActivity -> List ZoneActivity
applianceState "ON" activities  = ?filter_device_state_is_on
applianceState "OFF" activities = ?filter_device_state_is_off


-- Should return device list ?
export
handleCriteria : Maybe Criteria -> List ZoneActivity
handleCriteria Nothing        = []
handleCriteria (Just FirstIn) = ordinalEntry ?current_user ?uz_activity
handleCriteria (Just LastOut) = ordinalEntry ?current_user ?uz_activity
-- Maybe Some IO for mock fetching appliance state?
handleCriteria (Just ACOn)    = applianceState "ON" ?device_list
handleCriteria (Just ACOff)   = applianceState "OFF" ?device_list
handleCriteria (Just Always)  = ?device_list



export
parseCriteria : String -> Maybe Criteria
parseCriteria "first_in" = Just FirstIn
parseCriteria "last_out" = Just LastOut
parseCriteria "ac_on"    = Just ACOn
parseCriteria "ac_off"   = Just ACOff
parseCriteria "always"   = Just Always
parseCriteria _          = Nothing
