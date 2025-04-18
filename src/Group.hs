{-
Module Name: Group.hs
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Defines the `Group` data structure and utility functions to extract scheduling 
and equipment requirements for each group in the assignment tool.

Module Summary:
This module introduces the `Group` data type which represents a group of participants,
along with relevant accessor functions for scheduling, equipment needs, and floor preferences.

Key Functions:
- `getGroupID`, `getSize`, `getStartTime`, `getEndTime`
- `needsProjector`, `needsComputer`, `needsWheelchair`
- `getFloorPreference`, `hasFloorPreference`
- `printGroupDetails`

Dependencies:
- Data.Time (for working with UTCTime)

Known/Suspected Errors:
- None known at this time.
-}

module Group (
  Group(..),
  getGroupID, getSize, getStartTime, getEndTime,
  needsProjector, needsComputer, needsWheelchair,
  getFloorPreference
) where

import Data.Time (UTCTime)

-- | Group
-- Represents a participant group with scheduling details and equipment requirements.
data Group = Group
  { groupID         :: String        -- ^ Unique identifier for the group
  , size            :: Int           -- ^ Number of participants in the group
  , startTime       :: UTCTime       -- ^ Group's requested start time
  , endTime         :: UTCTime       -- ^ Group's requested end time
  , projectorNeed   :: Bool          -- ^ True if the group needs a projector
  , computerNeed    :: Bool          -- ^ True if the group needs a computer
  , wheelchairNeed  :: Bool          -- ^ True if accessibility is required
  , floorPreference :: Int           -- ^ Preferred floor (-1 = no preference)
  } deriving (Show, Eq)

-- | getGroupID
-- Returns the group's unique identifier.
getGroupID :: Group -> String
getGroupID = groupID

-- | getSize
-- Returns the number of participants in the group.
getSize :: Group -> Int
getSize = size

-- | getStartTime
-- Returns the group's scheduled start time.
getStartTime :: Group -> UTCTime
getStartTime = startTime

-- | getEndTime
-- Returns the group's scheduled end time.
getEndTime :: Group -> UTCTime
getEndTime = endTime

-- | needsProjector
-- Checks whether the group requires a projector.
needsProjector :: Group -> Bool
needsProjector = projectorNeed

-- | needsComputer
-- Checks whether the group requires a computer.
needsComputer :: Group -> Bool
needsComputer = computerNeed

-- | needsWheelchair
-- Checks whether the group requires wheelchair accessibility.
needsWheelchair :: Group -> Bool
needsWheelchair = wheelchairNeed

-- | getFloorPreference
-- Retrieves the preferred floor of the group.
-- Returns -1 if no preference is specified.
getFloorPreference :: Group -> Int
getFloorPreference = floorPreference
