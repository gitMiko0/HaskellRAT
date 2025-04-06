{- 
Module Name: Constraints.hs
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Contains all constraint validation logic to determine whether a group can be assigned to a room.

Module Summary:
Provides modular checks for each assignment constraint including floor preference,
room capacity, accessibility, equipment requirements, and time conflict detection.
Now includes a user-defined time gap (in minutes, converted internally to seconds)
to ensure a buffer between scheduled group assignments.

Key Functions:
- isValidAssignment
- checkFloorPreference
- checkRoomCapacity
- checkWheelchairAccess
- checkEquipment
- checkScheduleConflict
- checkTimeOverlap

Dependencies:
- Room.hs
- Group.hs
- Data.Time (for comparing and offsetting time schedules)

Known/Suspected Errors:
- None currently known
-}

module Constraints (
  isValidAssignment,
  checkFloorPreference,
  checkRoomCapacity,
  checkWheelchairAccess,
  checkEquipment,
  checkScheduleConflict,
  checkTimeOverlap
) where

import Room
import Group
import Data.Time (UTCTime, NominalDiffTime, addUTCTime)


{-|
isValidAssignment  
  Aggregates all constraint checks and returns True only if all are satisfied
  for assigning a group to a room, including time conflict detection with a
  user-defined buffer between bookings.

Parameters:
  Group             - the group being considered  
  Room              - the room being considered  
  NominalDiffTime   - required time gap between bookings (in seconds)

Return Value:
  Bool - True if all constraints pass
-}
isValidAssignment :: Group -> Room -> NominalDiffTime -> Bool
isValidAssignment group room gap =
  checkFloorPreference group room &&
  checkRoomCapacity group room &&
  checkWheelchairAccess group room &&
  checkEquipment group room &&
  checkScheduleConflict group room gap
  
{-|
checkFloorPreference  
  Verifies whether the group's floor preference matches the room's floor level.
  If the group has no preference (-1), this check always passes.

Parameters:
  Group - group requesting room  
  Room  - candidate room for assignment

Return Value:
  Bool - True if floor level is acceptable
-}
checkFloorPreference :: Group -> Room -> Bool
checkFloorPreference group room =
  let pref = getFloorPreference group
  in pref == -1 || pref == getFloorLevel room

{-|
checkRoomCapacity  
  Ensures that the room has enough capacity for the group's size.

Parameters:
  Group - group requesting room  
  Room  - candidate room

Return Value:
  Bool - True if room can hold the group
-}
checkRoomCapacity :: Group -> Room -> Bool
checkRoomCapacity group room = getCapacity room >= getSize group

{-|
checkWheelchairAccess  
  Checks if the room satisfies the group's wheelchair accessibility requirement.

Parameters:
  Group - group requesting room  
  Room  - candidate room

Return Value:
  Bool - True if wheelchair needs are satisfied
-}
checkWheelchairAccess :: Group -> Room -> Bool
checkWheelchairAccess group room =
  not (needsWheelchair group) || hasWheelchairAccess room

{-|
checkEquipment  
  Verifies that the room contains all equipment the group requires.

Parameters:
  Group - group requesting room  
  Room  - candidate room

Return Value:
  Bool - True if all required equipment is available
-}
checkEquipment :: Group -> Room -> Bool
checkEquipment group room =
  (not (needsProjector group) || hasProjector room) &&
  (not (needsComputer group) || hasComputer room)

{-|
checkScheduleConflict  
  Ensures that the group's schedule does not conflict with any existing bookings
  in the room, accounting for a user-defined time gap between assignments.

Parameters:
  Group             - group to check  
  Room              - room whose schedule will be compared  
  NominalDiffTime   - required time gap between bookings (converted from user-defined minutes)

Return Value:
  Bool - True if there are no overlaps, including the required buffer
-}
checkScheduleConflict :: Group -> Room -> NominalDiffTime -> Bool
checkScheduleConflict group room gap =
  all (\existing -> checkTimeOverlap (getStartTime group, getEndTime group) existing gap)
      (getSchedule room)

{-|
checkTimeOverlap  
  Compares a new time block with an existing one and determines if they overlap,
  using a required buffer time between them.

Parameters:
  (UTCTime, UTCTime)             - start and end of the new booking  
  (UTCTime, UTCTime, String)     - existing booking (with group ID)  
  NominalDiffTime                - required time gap in seconds

Return Value:
  Bool - True if no overlap exists with the specified buffer applied
-}
checkTimeOverlap :: (UTCTime, UTCTime) -> (UTCTime, UTCTime, String) -> NominalDiffTime -> Bool
checkTimeOverlap (newStart, newEnd) (existingStart, existingEnd, _) gap =
  addUTCTime gap newEnd <= existingStart || newStart >= addUTCTime gap existingEnd
