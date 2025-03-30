module Solver ( 
    isValidAssignment, 
    checkFloorPreference, 
    checkRoomCapacity, 
    checkWheelchairAccess, 
    checkEquipment, 
    checkTimeOverlap, 
    assignRooms,
    replaceRoom,
) where


import Room
import Group
import Data.Time (UTCTime)
import Data.List (find)

type Solution = [(String, String, UTCTime, UTCTime)]  -- (GroupID, RoomID, Start, End)

-- Individual constraint checks
checkRoomCapacity :: Group -> Room -> Bool
checkRoomCapacity group room = capacity room >= size group

checkWheelchairAccess :: Group -> Room -> Bool
checkWheelchairAccess group room = not (wheelchairNeed group) || wheelchairAccess room

checkEquipment :: Group -> Room -> Bool
checkEquipment group room =
  (not (projectorNeed group) || projector room) &&
  (not (computerNeed group) || computer room)

checkFloorPreference :: Group -> Room -> Bool
checkFloorPreference group room = maybe True (== floorLevel room) (floorPreference group)

checkScheduleConflict :: Group -> Room -> Bool
checkScheduleConflict group room =
  all (checkTimeOverlap (startTime group, endTime group)) (schedule room)

-- Check if a new booking overlaps with existing ones
checkTimeOverlap :: (UTCTime, UTCTime) -> (UTCTime, UTCTime, String) -> Bool
checkTimeOverlap (newStart, newEnd) (existingStart, existingEnd, _) =
  newEnd <= existingStart || newStart >= existingEnd

-- Main function that combines all constraints
isValidAssignment :: Group -> Room -> Bool
isValidAssignment group room =
  checkRoomCapacity group room &&
  checkWheelchairAccess group room &&
  checkEquipment group room &&
  checkFloorPreference group room &&
  checkScheduleConflict group room


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
