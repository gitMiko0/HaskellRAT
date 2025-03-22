-- Room.hs
{-# LANGUAGE DeriveGeneric #-}
module Room
    ( Room(..)
    , parseRooms
    ) where

import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), (.:))
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Vector as V
import Group (Group)

data Room = Room
    { roomId           :: !Text
    , capacity         :: !Int
    , wheelchairAccess :: !Bool
    , projector        :: !Bool
    , computer         :: !Bool
    , floorLevel       :: !Int
    , schedule         :: ![(UTCTime, UTCTime, Group)]
    } deriving (Show, Eq, Generic)

instance FromNamedRecord Room where
    parseNamedRecord m = Room
        <$> m .: "RoomID"
        <*> m .: "Capacity"
        <*> m .: "WheelchairAccess"
        <*> m .: "Projector"
        <*> m .: "Computer"
        <*> m .: "FloorLevel"
        <*> pure []  -- Initialize with empty schedule

parseRooms :: FilePath -> IO (Either String (V.Vector Room))
parseRooms = -- CSV parsing implementation using cassava
