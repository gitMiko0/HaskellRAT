module Solver (
  Solution,
  isValidAssignment,
  checkFloorPreference,
  checkRoomCapacity,
  checkWheelchairAccess,
  checkEquipment,
  checkTimeOverlap,
  assignRooms,
  replaceRoom
) where

import Room
import Group
import Data.Time (UTCTime)

-- | A solution is a list of tuples representing successful group-to-room assignments:
--   (GroupID, RoomID, StartTime, EndTime)
type Solution = [(String, String, UTCTime, UTCTime)]

-- === Constraint Check Functions ===

-- | Checks if the room matches the group's floor preference.
--   Groups with no preference (-1) pass automatically.
checkFloorPreference :: Group -> Room -> Bool
checkFloorPreference group room =
  let pref = getFloorPreference group
  in pref == -1 || pref == getFloorLevel room

-- | Checks if the room has sufficient capacity for the group size.
checkRoomCapacity :: Group -> Room -> Bool
checkRoomCapacity group room = getCapacity room >= getSize group

-- | Checks if the room meets wheelchair accessibility requirements.
checkWheelchairAccess :: Group -> Room -> Bool
checkWheelchairAccess group room =
  not (needsWheelchair group) || hasWheelchairAccess room

-- | Checks if the room satisfies projector and computer equipment needs.
checkEquipment :: Group -> Room -> Bool
checkEquipment group room =
  (not (needsProjector group) || hasProjector room) &&
  (not (needsComputer group) || hasComputer room)

-- | Checks for schedule overlap between a group's requested time and
--   existing bookings in the room.
checkScheduleConflict :: Group -> Room -> Bool
checkScheduleConflict group room =
  all (checkTimeOverlap (getStartTime group, getEndTime group)) (getSchedule room)

-- | Ensures a time block does not conflict with an existing one.
--   A 10-minute gap is assumed to be pre-applied in input data.
checkTimeOverlap :: (UTCTime, UTCTime) -> (UTCTime, UTCTime, String) -> Bool
checkTimeOverlap (newStart, newEnd) (existingStart, existingEnd, _) =
  newEnd <= existingStart || newStart >= existingEnd

-- | Validates whether all constraints are met for assigning a group to a room.
isValidAssignment :: Group -> Room -> Bool
isValidAssignment group room =
  checkFloorPreference group room &&
  checkRoomCapacity group room &&
  checkWheelchairAccess group room &&
  checkEquipment group room &&
  checkScheduleConflict group room

-- === Recursive Assignment Solver ===

-- | Recursively assigns groups to rooms using backtracking.
--   Returns a list of valid solutions (should contain only one in this project).
assignRooms :: [Group] -> [Room] -> [Solution]
assignRooms [] _ = [[]]  -- Base case: all groups have been assigned
assignRooms (g:gs) rooms =
  [ (getGroupID g, getRoomID r, getStartTime g, getEndTime g) : sol
  | r <- rooms, isValidAssignment g r
  , let updatedRoom = addSchedule r (getStartTime g, getEndTime g, getGroupID g)
  , sol <- assignRooms gs (replaceRoom updatedRoom rooms)
  ]

-- | Replaces a room in the room list with an updated version (e.g., new schedule).
replaceRoom :: Room -> [Room] -> [Room]
replaceRoom updated = map (\r -> if getRoomID r == getRoomID updated then updated else r)
