module InputReader where

import Room
import Group
import Data.Time
import System.IO
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Text.Read (readMaybe)  -- Import readMaybe

-- Helper function to parse a line from CSV
parseCSVLine :: String -> [String]
parseCSVLine = splitOn ","

-- Convert a string to a boolean
parseBool :: String -> Bool
parseBool "1" = True
parseBool _   = False

-- Convert String to UTCTime with specific format
-- Note that Date format is "YYYY-MM-DD HH:MM" (24-hour clock) and hours <10 MUST have trailing zeroes (ie. 01:00)
parseDateTime :: String -> Maybe UTCTime
parseDateTime dateStr =
  parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" (trim dateStr)

-- Load room data from a CSV file
loadRooms :: FilePath -> IO [Room]
loadRooms file = do
  contents <- readFile file
  let (header:lineList) = tail (lines contents)  -- Skip the header
  return $ map parseRoom lineList

parseRoom :: String -> Room
parseRoom line =
  let [rid, cap, wca, proj, comp, floor] = parseCSVLine line
      cap' = cap        -- Debugging line
      floor' = floor    -- Debugging line
  in Room rid
       (fromMaybe (error "Invalid capacity") (readMaybe (trim cap') :: Maybe Int))
       (parseBool wca)
       (parseBool proj)
       (parseBool comp)
       (fromMaybe (error "Invalid floor level") (readMaybe (trim floor') :: Maybe Int))
       []

-- Remove leading and trailing spaces from a string
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (== ' ')

-- Load group data from a CSV file
loadGroups :: FilePath -> IO [Group]
loadGroups file = do
  contents <- readFile file
  let (header:lineList) = tail (lines contents)  -- Skip the header
  return $ map parseGroup lineList

parseGroup :: String -> Group
parseGroup line =
  let [gid, size, wca, proj, comp, floor, start, end] = parseCSVLine line
      trimmedStart = trim start
      trimmedEnd = trim end
      startTime = parseDateTime trimmedStart
      endTime = parseDateTime trimmedEnd
  in case (startTime, endTime) of
      (Just s, Just e) -> Group gid
                            (read size)
                            s
                            e
                            (parseBool proj)
                            (parseBool comp)
                            (parseBool wca)
                            (if floor == "-1" then Nothing else Just (read floor))
      _ -> error ("Invalid date format: " ++ start ++ " / " ++ end)

