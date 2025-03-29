module InputReader where

import Room
import Group
import Data.Time
import System.IO
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Text.Read (readMaybe)  -- Import readMaybe

-- General function to load and parse a CSV file
loadCSV :: (String -> a) -> FilePath -> IO [a]
loadCSV parseFunc file = do
  contents <- readFile file
  let lineList = drop 1 (lines contents)  -- Skip the header
  return $ map parseFunc lineList

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

parseGroup :: String -> Group
parseGroup line =
  let [gid, size, wca, proj, comp, floor, start, end] = parseCSVLine line
      trimmedStart = trim start
      trimmedEnd = trim end
      startTime = parseDateTime trimmedStart
      endTime = parseDateTime trimmedEnd
  in case (startTime, endTime) of
      (Just start, Just end) -> Group gid
                            (read size)
                            start
                            end
                            (parseBool proj)
                            (parseBool comp)
                            (parseBool wca)
                            (if floor == "-1" then Nothing else Just (read floor))
      _ -> error ("Invalid date format: " ++ start ++ " / " ++ end)

