module Tests.Helpers where

import Room
import Group
import Data.Time
import Data.Time.Format (defaultTimeLocale, parseTimeM)

group :: String -> String -> Int -> Bool -> Bool -> Bool -> Int -> String -> String -> Group
group start end size wheelchair projector computer floor dateStr groupId =
    let date = parseDate dateStr
        startTime = parseTimeOfDay start
        endTime = parseTimeOfDay end
    in Group groupId size (dateTime date startTime) (dateTime date endTime) projector computer wheelchair floor

room :: String -> Int -> Bool -> Bool -> Bool -> Int -> Room
room roomId capacity wheelchair projector computer floor =
    Room roomId capacity wheelchair projector computer floor []

dateTime :: Day -> TimeOfDay -> UTCTime
dateTime date time = UTCTime date (timeOfDayToTime time)

parseDate :: String -> Day
parseDate s = case parseTimeM True defaultTimeLocale "%F" s of
    Just d -> d
    Nothing -> error $ "Invalid date format: " ++ s

parseTimeOfDay :: String -> TimeOfDay
parseTimeOfDay s = case parseTimeM True defaultTimeLocale "%R" s of
    Just t -> t
    Nothing -> error $ "Invalid time format: " ++ s

testGap :: NominalDiffTime
testGap = 600  -- 10 minutes
