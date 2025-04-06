{-
Module Name: InputReader.hs
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Responsible for reading CSV input files and parsing them into Group and Room data structures.

Module Summary:
This module provides functionality to read and parse lines from room and group CSV files using custom parsing rules.
It handles boolean fields, dates, trimming spaces, and gracefully reports format errors for robust preprocessing.

Key Functions:
- `loadCSV`, `parseRoom`, `parseGroup`

Dependencies:
- Room.hs (Room data type)
- Group.hs (Group data type)
- Data.Time (for parsing UTCTime)
- Text.Read, Data.List.Split, Data.Maybe

Known/Suspected Errors:
- None so far
-}

module InputReader (
  loadCSV,
  parseRoom,
  parseGroup,
  runWithGap
) where


import Room
import Group
import OutputWriter (writeCSV, formatSolution)
import Solver (assignRooms)
import Data.Time
import System.IO
import Data.Char (toUpper)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

{-
loadCSV
  Loads a CSV file and applies a parsing function to each non-header line.

Parameters:
  parseFunc :: String -> a
    A function that parses a line of CSV into a desired type (Room or Group)

  file :: FilePath
    Path to the input CSV file

Return Value:
  IO [a] - List of parsed values of type `a`
-}
loadCSV :: (String -> a) -> FilePath -> IO [a]
loadCSV parseFunc file = do
  contents <- readFile file
  let lineList = drop 1 (lines contents)  -- Skip the header
  return $ map parseFunc lineList

{-
parseCSVLine
  Splits a CSV line into individual, trimmed string values.

Parameters:
  String - A raw CSV line

Return Value:
  [String] - Cleaned list of values from the line
-}
parseCSVLine :: String -> [String]
parseCSVLine = map trim . splitOn ","

{-|
parseBoolOrFail  
  Strictly parses a string into a Bool. Only "TRUE" or "FALSE" (case-insensitive) are accepted.
  Raises an error for any other value.

Parameters:
  fieldName :: String - The name of the field being parsed (used in error message)
  raw       :: String - The raw input string

Return Value:
  Bool - Parsed boolean value

Exceptions:
  Raises "Invalid boolean for <fieldName>: <value>" if the input is invalid.
-}

parseBoolOrFail :: String -> String -> Bool
parseBoolOrFail fieldName raw =
  case map toUpper (trim raw) of
    "TRUE"  -> True
    "FALSE" -> False
    other   -> error $ "Invalid boolean for " ++ fieldName ++ ": " ++ show other


{-
parseDateTime
  Parses a string in the format "YYYY-MM-DD HH:MM" to UTCTime.

Parameters:
  String - A datetime string from CSV

Return Value:
  Maybe UTCTime - Parsed time or Nothing on failure
-}
parseDateTime :: String -> Maybe UTCTime
parseDateTime dateStr =
  parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" (trim dateStr)

{-
trim
  Removes leading and trailing whitespace from a string.

Parameters:
  String - Input string

Return Value:
  String - Trimmed output
-}
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

{-|
parseRoom  
  Converts a line from the room CSV file into a Room data structure.

Parameters:
  String - CSV line containing RoomID, Capacity, WheelchairAccess, Projector, Computer, FloorLevel

Return Value:
  Room - Parsed Room record

Exceptions:
  Raises errors for:
    - Invalid capacity
    - Invalid floor level
    - Invalid boolean values

Note:
  Uses Bang Patterns to force evaluation of parsed values to ensure that all malformed input
  raises errors at parse-time rather than later due to Haskell's lazy evaluation.
-}

parseRoom :: String -> Room
parseRoom line =
  let [rid, cap, wca, proj, comp, floor] = parseCSVLine line
      !capacity   = readOrFail "Invalid capacity" cap
      !level      = readOrFail "Invalid floor level" floor
      !wheelchair = parseBoolOrFail "WheelchairAccess" wca
      !projector  = parseBoolOrFail "Projector" proj
      !computer   = parseBoolOrFail "Computer" comp
  in Room rid capacity wheelchair projector computer level []


{-
parseGroup
  Converts a line from the group CSV file into a Group data structure.

Parameters:
  String - CSV line containing GroupID, Size, WheelchairAccess, Projector, Computer, FloorPreference, Start, End

Return Value:
  Group - Parsed Group record

Exceptions:
  Raises an error on malformed date strings or integer parsing failure.
-}
parseGroup :: String -> Group
parseGroup line =
  let [gid, size, wca, proj, comp, floor, start, end] = parseCSVLine line
      !pref       = readOrFail "Invalid floor preference" floor
      !groupSize  = readOrFail "Invalid size" size
      !wheelchair = parseBoolOrFail "WheelchairAccess" wca
      !projector  = parseBoolOrFail "Projector" proj
      !computer   = parseBoolOrFail "Computer" comp
      startTime   = parseDateTime start
      endTime     = parseDateTime end
  in case (startTime, endTime) of
      (Just st, Just et) ->
        Group gid groupSize st et projector computer wheelchair pref
      _ -> error $ "Invalid date format: " ++ start ++ " / " ++ end


{-
readIntOrFail
  Attempts to parse a string as an integer or raises a custom error message.

Parameters:
  errMsg :: String - Message to show if parsing fails
  s      :: String - Input string to parse

Return Value:
  Int - Parsed integer or triggers error
-}
readOrFail :: String -> String -> Int
readOrFail errMsg s = fromMaybe (error errMsg) (readMaybe (trim s))

runWithGap :: FilePath -> FilePath -> NominalDiffTime -> IO ()
runWithGap roomsFile groupsFile gap = do
  rooms <- loadCSV parseRoom roomsFile
  groups <- loadCSV parseGroup groupsFile
  case assignRooms groups rooms gap of
    Just sol -> do
      putStrLn "Room Assignments:"
      putStrLn (formatSolution sol)
      writeCSV "assignments.csv" sol
    Nothing -> putStrLn "Error: Constraints cannot be satisfied with the provided input."