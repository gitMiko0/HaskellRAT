module Group (
  Group(..),
  getGroupID, getSize, getStartTime, getEndTime,
  needsProjector, needsComputer, needsWheelchair,
  getFloorPreference, hasFloorPreference,
  printGroupDetails
) where

import Data.Time (UTCTime, formatTime, defaultTimeLocale)

-- | Represents a participant group with scheduling and equipment needs.
data Group = Group
  { groupID         :: String         -- ^ Unique identifier for the group
  , size            :: Int            -- ^ Number of participants in the group
  , startTime       :: UTCTime        -- ^ Group's scheduled start time
  , endTime         :: UTCTime        -- ^ Group's scheduled end time
  , projectorNeed   :: Bool           -- ^ True if the group needs a projector
  , computerNeed    :: Bool           -- ^ True if the group needs a computer
  , wheelchairNeed  :: Bool           -- ^ True if the group needs accessibility support
  , floorPreference :: Int            -- ^ Preferred floor (-1 means no preference)
  } deriving (Show, Eq)

-- === Getters ===

-- | Returns the group's unique identifier.
getGroupID :: Group -> String
getGroupID = groupID

-- | Returns the number of participants in the group.
getSize :: Group -> Int
getSize = size

-- | Returns the group's scheduled start time.
getStartTime :: Group -> UTCTime
getStartTime = startTime

-- | Returns the group's scheduled end time.
getEndTime :: Group -> UTCTime
getEndTime = endTime

-- | Checks if the group needs a projector.
needsProjector :: Group -> Bool
needsProjector = projectorNeed

-- | Checks if the group needs a computer.
needsComputer :: Group -> Bool
needsComputer = computerNeed

-- | Checks if the group requires wheelchair accessibility.
needsWheelchair :: Group -> Bool
needsWheelchair = wheelchairNeed

-- | Returns the preferred floor (-1 = no preference).
getFloorPreference :: Group -> Int
getFloorPreference = floorPreference

-- | Returns True if the group has a specific floor preference.
hasFloorPreference :: Group -> Bool
hasFloorPreference g = getFloorPreference g /= -1

-- === Debugging Utility ===

-- | Prints the details of a group in a human-readable format.
-- | Prints the details of a group in a human-readable format.
printGroupDetails :: Group -> IO ()
printGroupDetails g =
  putStrLn $ "Group " ++ getGroupID g ++
    ", Size: " ++ show (getSize g) ++
    ", Time: " ++ show (getStartTime g) ++ " - " ++ show (getEndTime g) ++
    ", Equipment: " ++ eqStr ++
    ", Floor Pref: " ++ if hasFloorPreference g then show (getFloorPreference g) else "None"
  where
    eqStr = concat $
      [if needsProjector g then "[Projector] " else ""
      ,if needsComputer g then "[Computer] " else ""
      ,if needsWheelchair g then "[Wheelchair] " else ""]

