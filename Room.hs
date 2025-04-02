module Room where

import Data.Time (UTCTime)

data Room = Room
  { roomID            :: String
  , capacity          :: Int
  , wheelchairAccess  :: Bool
  , projector         :: Bool
  , computer          :: Bool
  , floorLevel        :: Int
  , schedule          :: [(UTCTime, UTCTime, String)]
  } deriving (Show, Eq)

-- Encapsulation: Getters
getRoomID :: Room -> String
getRoomID = roomID

getCapacity :: Room -> Int
getCapacity = capacity

hasWheelchairAccess :: Room -> Bool
hasWheelchairAccess = wheelchairAccess

hasProjector :: Room -> Bool
hasProjector = projector

hasComputer :: Room -> Bool
hasComputer = computer

getFloorLevel :: Room -> Int
getFloorLevel = floorLevel

getSchedule :: Room -> [(UTCTime, UTCTime, String)]
getSchedule = schedule

-- Encapsulation: Setters
setSchedule :: [(UTCTime, UTCTime, String)] -> Room -> Room
setSchedule newSchedule room = room { schedule = newSchedule }

addSchedule :: Room -> (UTCTime, UTCTime, String) -> Room
addSchedule room newEntry = setSchedule (newEntry : getSchedule room) room
