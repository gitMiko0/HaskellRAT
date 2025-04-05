module Room (
  Room(..),
  getRoomID, getCapacity, hasWheelchairAccess,
  hasProjector, hasComputer, getFloorLevel,
  getSchedule, setSchedule, addSchedule
) where

import Data.Time (UTCTime)

-- | Represents a room with its attributes and scheduled time blocks.
data Room = Room
  { roomID           :: String                         -- ^ Unique room identifier
  , capacity         :: Int                            -- ^ Maximum capacity
  , wheelchairAccess :: Bool                           -- ^ Whether the room is wheelchair accessible
  , projector        :: Bool                           -- ^ Whether the room has a projector
  , computer         :: Bool                           -- ^ Whether the room has a computer
  , floorLevel       :: Int                            -- ^ Floor number the room is on
  , schedule         :: [(UTCTime, UTCTime, String)]   -- ^ List of bookings: (start, end, groupID)
  } deriving (Show, Eq)

-- === Getters ===

-- | Returns the room's unique ID.
getRoomID :: Room -> String
getRoomID = roomID

-- | Returns the maximum capacity of the room.
getCapacity :: Room -> Int
getCapacity = capacity

-- | Checks if the room is wheelchair accessible.
hasWheelchairAccess :: Room -> Bool
hasWheelchairAccess = wheelchairAccess

-- | Checks if the room has a projector.
hasProjector :: Room -> Bool
hasProjector = projector

-- | Checks if the room has a computer.
hasComputer :: Room -> Bool
hasComputer = computer

-- | Returns the floor level of the room.
getFloorLevel :: Room -> Int
getFloorLevel = floorLevel

-- | Retrieves the current booking schedule of the room.
getSchedule :: Room -> [(UTCTime, UTCTime, String)]
getSchedule = schedule

-- === Setters ===

-- | Replaces the schedule of a room with a new one.
setSchedule :: [(UTCTime, UTCTime, String)] -> Room -> Room
setSchedule newSched room = room { schedule = newSched }

-- | Adds a new booking to the beginning of the room's schedule.
addSchedule :: Room -> (UTCTime, UTCTime, String) -> Room
addSchedule room newEntry = room { schedule = newEntry : schedule room }
