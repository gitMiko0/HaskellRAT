{- 
Module Name: Solver.hs
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Implements the recursive backtracking algorithm that assigns groups to rooms
based on pre-defined constraints such as schedule availability and room features.

Module Summary:
This module defines the `Solution` type and provides the core logic to assign
groups to rooms while ensuring all constraints are satisfied.

Key Functions:
- `assignRooms`
- `replaceRoom`

Dependencies:
- Room.hs
- Group.hs
- Constraints.hs

Known/Suspected Errors:
- None currently known
-}

module Solver (
  Solution,
  assignRooms,
  replaceRoom
) where

import Room
import Group
import Constraints
import Data.Time (UTCTime, NominalDiffTime)

-- | A Solution is a list of successful group-room-time assignments
type Solution = [(String, String, UTCTime, UTCTime)]

-- | Recursively assigns each group to a valid room using backtracking, returning the first valid solution found.
assignRooms :: [Group] -> [Room] -> NominalDiffTime -> Maybe Solution
assignRooms [] _ _ = Just []  -- Base case: all groups assigned
assignRooms (g:gs) rooms timeGap = tryRooms rooms
  where
    tryRooms [] = Nothing
    tryRooms (r:rs)
      | isValidAssignment g r timeGap =
          let updatedRoom = addSchedule r (getStartTime g, getEndTime g, getGroupID g)
              updatedRooms = replaceRoom updatedRoom rooms
          in case assignRooms gs updatedRooms timeGap of
               Just sol -> Just ((getGroupID g, getRoomID r, getStartTime g, getEndTime g) : sol)
               Nothing  -> tryRooms rs -- Path fails, backtrack
      | otherwise = tryRooms rs

-- | Replaces a room in the list with an updated version by matching RoomID
replaceRoom :: Room -> [Room] -> [Room]
replaceRoom updated = map (\r -> if getRoomID r == getRoomID updated then updated else r)
