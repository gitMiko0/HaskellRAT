module Solver where

import Room
import Group
import Data.Time (UTCTime)
import Data.List (find)

type Solution = [(String, String, UTCTime, UTCTime)]  -- (GroupID, RoomID, Start, End)

-- Check if a room satisfies all constraints for a given group
isValidAssignment :: Group -> Room -> Bool
isValidAssignment group room =
  capacity room >= size group &&
  (not (wheelchairNeed group) || wheelchairAccess room) &&
  (not (projectorNeed group) || projector room) &&
  (not (computerNeed group) || computer room) &&
  (maybe True (== floorLevel room) (floorPreference group)) &&
  all (noTimeConflict (startTime group, endTime group)) (schedule room)

-- Check if a new booking overlaps with existing ones
noTimeConflict :: (UTCTime, UTCTime) -> (UTCTime, UTCTime, String) -> Bool
noTimeConflict (newStart, newEnd) (existingStart, existingEnd, _) =
  newEnd <= existingStart || newStart >= existingEnd

-- Assign groups to rooms using backtracking
assignRooms :: [Group] -> [Room] -> [Solution]
assignRooms [] _ = [[]]  -- Base case: all groups assigned
assignRooms (g:gs) rooms =
  [ (groupID g, roomID r, startTime g, endTime g) : sol
  | r <- rooms, isValidAssignment g r
  , let updatedRoom = r { schedule = (startTime g, endTime g, groupID g) : schedule r }
  , sol <- assignRooms gs (replaceRoom updatedRoom rooms)
  ]

-- Replace a room in the list after updating its schedule
replaceRoom :: Room -> [Room] -> [Room]
replaceRoom updated = map (\r -> if roomID r == roomID updated then updated else r)
