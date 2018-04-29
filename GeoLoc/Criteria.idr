module Criteria

import Zone

%access public export


data Criteria
  = FirstIn
  | LastOut
  | ACOn
  | ACOff
  | Always


userInZone : UserZoneActivity -> Bool
userInZone (Enter userZone) = entered && enterOnce && notCurrUser
                              where
                                entered     = enter userZone
                                enterOnce   = 10 > enter_at userZone
                                notCurrUser = user_id userZone /= 2
userInZone (Leave _)        = False


ordinalEntry : List UserZoneActivity -> List UserZoneActivity
ordinalEntry activities = filter userInZone activities


applianceState : String -> List UserZoneActivity -> List UserZoneActivity
applianceState "ON" activities  = ?filter_device_state_is_on
applianceState "OFF" activities = ?filter_device_state_is_off


-- Should return device list ?
handleCriteria : Maybe Criteria -> List UserZoneActivity
handleCriteria Nothing        = []
handleCriteria (Just FirstIn) = ordinalEntry ?uz_activity
handleCriteria (Just LastOut) = ordinalEntry ?uz_activity
-- Maybe Some IO for fetching appliance state?
handleCriteria (Just ACOn)    = applianceState "ON" ?device_list
handleCriteria (Just ACOff)   = applianceState "OFF" ?device_list
handleCriteria (Just Always)  = ?always


parseCriteria : String -> Maybe Criteria
parseCriteria "first_in" = Just FirstIn
parseCriteria "last_out" = Just LastOut
parseCriteria "ac_on"    = Just ACOn
parseCriteria "ac_off"   = Just ACOff
parseCriteria "always"   = Just Always
parseCriteria _          = Nothing


matchCriteriaWith : String -> Integer -> Integer -> List Integer
matchCriteriaWith criteriaStr userID timestamp = ?matchCriteriaWith_rhs
  where
    criteria : Maybe Criteria
    criteria = parseCriteria criteriaStr
