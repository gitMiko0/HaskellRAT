{-
Module Name: Room.hs
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Defines the `Room` data structure and functions for managing room properties 
and scheduled group bookings.

Module Summary:
This module introduces the `Room` type which stores information about each physical room's 
capacity, accessibility, equipment availability, and assigned group bookings. 

Key Functions:
- getRoomID, getCapacity, hasWheelchairAccess, hasProjector, hasComputer
- getFloorLevel, getSchedule
- setSchedule, addSchedule

Dependencies:
- Data.Time (for working with UTCTime)

Known/Suspected Errors:
- None currently known
-}

module Room (
  Room(..),
  getRoomID, getCapacity, hasWheelchairAccess,
  hasProjector, hasComputer, getFloorLevel,
  getSchedule, setSchedule, addSchedule
) where

import Data.Time (UTCTime)

{-|
  Room
  Represents a room with key characteristics such as ID, capacity, accessibility features, 
  and a list of scheduled bookings.

  Fields:
    roomID (String)                         - Unique identifier for the room
    capacity (Int)                          - Maximum allowed group size
    wheelchairAccess (Bool)                 - Indicates if the room supports accessibility needs
    projector (Bool)                        - Indicates if the room has a projector
    computer (Bool)                         - Indicates if the room has a computer
    floorLevel (Int)                        - The floor number of the room
    schedule ([(UTCTime, UTCTime, String)]) - Current list of bookings with time range and group ID
-}
data Room = Room
  { roomID           :: String
  , capacity         :: Int
  , wheelchairAccess :: Bool
  , projector        :: Bool
  , computer         :: Bool
  , floorLevel       :: Int
  , schedule         :: [(UTCTime, UTCTime, String)]
  } deriving (Show, Eq)

{- getRoomID
   Returns the unique identifier of the room.

   Parameters:
     Room - the room in question

   Return Value:
     String - the room ID
-}
getRoomID :: Room -> String
getRoomID = roomID

{- getCapacity
   Returns the maximum number of people allowed in the room.

   Parameters:
     Room

   Return Value:
     Int - the capacity
-}
getCapacity :: Room -> Int
getCapacity = capacity

{- hasWheelchairAccess
   Determines if the room is wheelchair accessible.

   Parameters:
     Room

   Return Value:
     Bool - True if accessible
-}
hasWheelchairAccess :: Room -> Bool
hasWheelchairAccess = wheelchairAccess

{- hasProjector
   Determines if the room has a projector.

   Parameters:
     Room

   Return Value:
     Bool - True if equipped with projector
-}
hasProjector :: Room -> Bool
hasProjector = projector

{- hasComputer
   Determines if the room has a computer.

   Parameters:
     Room

   Return Value:
     Bool - True if equipped with computer
-}
hasComputer :: Room -> Bool
hasComputer = computer

{- getFloorLevel
   Retrieves the floor level of the room.

   Parameters:
     Room

   Return Value:
     Int - the floor number
-}
getFloorLevel :: Room -> Int
getFloorLevel = floorLevel

{- getSchedule
   Returns the current list of bookings in the room.

   Parameters:
     Room

   Return Value:
     [(UTCTime, UTCTime, String)] - list of bookings
-}
getSchedule :: Room -> [(UTCTime, UTCTime, String)]
getSchedule = schedule

{- setSchedule
   Replaces the room’s current schedule with a new one.

   Parameters:
     [(UTCTime, UTCTime, String)] - new list of bookings
     Room - the room to update

   Return Value:
     Room - updated room with new schedule
-}
setSchedule :: [(UTCTime, UTCTime, String)] -> Room -> Room
setSchedule newSched room = room { schedule = newSched }

{- addSchedule
   Adds a new booking to the beginning of the room’s schedule.

   Parameters:
     Room - the room to update
     (UTCTime, UTCTime, String) - booking entry

   Return Value:
     Room - updated room with the new booking prepended
-}
addSchedule :: Room -> (UTCTime, UTCTime, String) -> Room
addSchedule room newEntry = room { schedule = newEntry : schedule room }
