module Group where

import Data.Time (UTCTime)

data Group = Group
  { groupID          :: String
  , size            :: Int
  , startTime       :: UTCTime
  , endTime         :: UTCTime
  , projectorNeed   :: Bool
  , computerNeed    :: Bool
  , wheelchairNeed  :: Bool
  , floorPreference :: Maybe Int  -- Nothing means no preference
  } deriving (Show, Eq)
