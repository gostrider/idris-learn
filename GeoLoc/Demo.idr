module Demo

import Zone
import Criteria
import DB


record ActionParams where
  constructor MkParams
  criteria : Criteria
  user_id : UserID
  timestamp : Timestamp


parseTimestamp : Integer -> Timestamp
parseTimestamp x =
  if x < 11
    then (ValidTimestamp x)
    else InvalidTimestamp


parseParams : String -> Integer -> Integer -> ActionParams
parseParams criteriaStr userID timestamp =
  let
    Just criteria' = Criteria.parseCriteria criteriaStr
    Just user = List.find (\u => idx u == userID) DB.users
    userID' = ValidUserID $ User.idx user
    time = parseTimestamp timestamp
  in
    MkParams criteria' userID' time


matchCriteriaWith : ActionParams -> List DeviceID
matchCriteriaWith params = ?matchCriteriaWith_rhs


program : List DeviceID
program =
  ?match_rhs $ parseParams "first_in" 1 1

{-

userID -> zoneID -> phoneUUID -> phoneID -> enter -> updated_on

program -> IO (List UserID)
program =
  matchCriteriaWith
  ("criteria" currentUser timestamp)
  ZoneActivity
  DeviceList?

-}
