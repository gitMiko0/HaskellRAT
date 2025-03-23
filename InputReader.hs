module InputReader where

import Room
import Group
import Data.Time
import System.IO
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)


-- Helper function to parse a line from CSV
parseCSVLine :: String -> [String]
parseCSVLine = splitOn ","

-- Convert a string to a boolean
parseBool :: String -> Bool
parseBool "1" = True
parseBool _   = False

-- Parse a date string into UTCTime
parseDateTime :: String -> Maybe UTCTime
parseDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M"


-- Load room data from a CSV file
loadRooms :: FilePath -> IO [Room]
loadRooms file = do
  contents <- readFile file
  return $ map parseRoom (lines contents)

parseRoom :: String -> Room
parseRoom line =
  let [rid, cap, wca, proj, comp, floor] = parseCSVLine line
  in Room rid (read cap) (parseBool wca) (parseBool proj) (parseBool comp) (read floor) []

-- Load group data from a CSV file
loadGroups :: FilePath -> IO [Group]
loadGroups file = do
  contents <- readFile file
  return $ map parseGroup (lines contents)

parseGroup :: String -> Group
parseGroup line =
  let [gid, size, wca, proj, comp, floor, start, end] = parseCSVLine line
  in Group gid
       (read size)
       (fromMaybe (error "Invalid start time") (parseDateTime start))
       (fromMaybe (error "Invalid end time") (parseDateTime end))
       (parseBool proj)
       (parseBool comp)
       (parseBool wca)
       (if floor == "-1" then Nothing else Just (read floor))
