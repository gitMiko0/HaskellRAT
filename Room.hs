module Room where

import Data.Time (UTCTime)

data Room = Room
  { roomID            :: String
  , capacity          :: Int
  , wheelchairAccess  :: Bool
  , projector         :: Bool
  , computer          :: Bool
  , floorLevel        :: Int
  , schedule          :: [(UTCTime, UTCTime, String)]  -- (Start, End, GroupID)
  } deriving (Show, Eq)
