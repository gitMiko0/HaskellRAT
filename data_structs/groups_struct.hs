-- Group.hs
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Group
    ( Group(..)
    , parseGroups
    ) where

import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), (.:))
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Vector as V

data Group = Group
    { groupId          :: !Text
    , start            :: !UTCTime
    , end              :: !UTCTime
    , size             :: !Int
    , wheelchairAccess :: !Bool
    , projector        :: !Bool
    , computer         :: !Bool
    , floorPreference  :: !Int
    } deriving (Show, Eq, Generic)

instance FromNamedRecord Group where
    parseNamedRecord m = Group
        <$> m .: "GroupID"
        <*> m .: "Start"
        <*> m .: "End"
        <*> m .: "Size"
        <*> m .: "WheelchairAccess"
        <*> m .: "Projector"
        <*> m .: "Computer"
        <*> m .: "FloorPreference"

parseGroups :: FilePath -> IO (Either String (V.Vector Group))
parseGroups = -- CSV parsing implementation using cassava
