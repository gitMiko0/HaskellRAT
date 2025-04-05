module InputReader (
  loadCSV,
  parseRoom,
  parseGroup
) where

import Room
import Group
import Data.Time
import System.IO
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- | Loads a CSV file using a custom row parser, skipping the first header line.
loadCSV :: (String -> a) -> FilePath -> IO [a]
loadCSV parseFunc file = do
  contents <- readFile file
  let lineList = drop 1 (lines contents)  -- Skip the header
  return $ map parseFunc lineList

-- | Parses a single line from CSV into a list of trimmed string values.
parseCSVLine :: String -> [String]
parseCSVLine = map trim . splitOn ","

-- | Converts a string to Bool. "1" = True, anything else = False.
parseBool :: String -> Bool
parseBool "1" = True
parseBool _   = False

-- | Parses a string in the format "YYYY-MM-DD HH:MM" to UTCTime.
--   Leading/trailing spaces are removed before parsing.
parseDateTime :: String -> Maybe UTCTime
parseDateTime dateStr =
  parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" (trim dateStr)

-- | Removes leading and trailing spaces from a string.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

-- | Parses a line of CSV into a Room record.
--   Expects fields: RoomID, Capacity, WheelchairAccess, Projector, Computer, FloorLevel
parseRoom :: String -> Room
parseRoom line =
  let [rid, cap, wca, proj, comp, floor] = parseCSVLine line
  in Room rid
       (readOrFail "Invalid capacity" cap)
       (parseBool wca)
       (parseBool proj)
       (parseBool comp)
       (readOrFail "Invalid floor level" floor)
       []

-- | Parses a line of CSV into a Group record.
--   Expects fields: GroupID, Size, WheelchairAccess, Projector, Computer, FloorPreference, Start, End
parseGroup :: String -> Group
parseGroup line =
  let [gid, size, wca, proj, comp, floor, start, end] = parseCSVLine line
      startTime = parseDateTime start
      endTime   = parseDateTime end
      pref      = readOrFail "Invalid floor preference" floor
  in case (startTime, endTime) of
      (Just st, Just et) ->
        Group gid
              (readOrFail "Invalid size" size)
              st
              et
              (parseBool proj)
              (parseBool comp)
              (parseBool wca)
              pref
      _ -> error $ "Invalid date format: " ++ start ++ " / " ++ end

-- | Safely reads an integer value or raises a user-friendly error.
readOrFail :: String -> String -> Int
readOrFail errMsg s = fromMaybe (error errMsg) (readMaybe (trim s))
