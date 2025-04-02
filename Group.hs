module Group where

import Data.Time (UTCTime)

data Group = Group
  { groupID          :: String
  , size            :: Int
  , startTime       :: UTCTime
  , endTime         :: UTCTime
  , projectorNeed   :: Bool
  , computerNeed    :: Bool
  , wheelchairNeed  :: Bool
  , floorPreference :: Maybe Int
  } deriving (Show, Eq)

-- Encapsulation: Getters
getGroupID :: Group -> String
getGroupID = groupID

getSize :: Group -> Int
getSize = size

getStartTime :: Group -> UTCTime
getStartTime = startTime

getEndTime :: Group -> UTCTime
getEndTime = endTime

needsProjector :: Group -> Bool
needsProjector = projectorNeed

needsComputer :: Group -> Bool
needsComputer = computerNeed

needsWheelchair :: Group -> Bool
needsWheelchair = wheelchairNeed

getFloorPreference :: Group -> Maybe Int
getFloorPreference = floorPreference
