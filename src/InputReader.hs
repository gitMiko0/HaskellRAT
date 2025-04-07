{-|
Module Name: InputReader.hs
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Responsible for reading CSV input files and parsing them into Group and Room data structures.

Module Summary:
This module provides functionality to read and parse lines from room and group CSV files using custom parsing rules.
It handles boolean fields, dates, trimming spaces, and gracefully reports format errors for robust preprocessing.

Key Functions:
- loadCSV
- parseRoom
- parseGroup
- runWithGap

Dependencies:
- Room.hs (Room data type)
- Group.hs (Group data type)
- Data.Time (for parsing UTCTime)
- Text.Read, Data.List.Split, Data.Maybe

Known/Suspected Errors:
- None known
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
import Solver (assignGroups)
import Data.Time
import Data.Char (toUpper)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Exception (try, IOException)
import System.IO

{-|
loadCSV
  Loads a CSV file and applies a parsing function to each non-header line.

Parameters:
  parseFunc :: String -> a
    A function that parses a line of CSV into a desired type (Room or Group)

  file :: FilePath
    Path to the input CSV file

Return Value:
  IO [a] - List of parsed values of type `a`

Exceptions:
  Handles and rethrows file read errors with a friendly message.
-}
loadCSV :: (String -> a) -> FilePath -> IO [a]
loadCSV parseFunc file = do
  result <- try (readFile file) :: IO (Either IOException String)
  case result of
    Left _ -> error $ "Error: File not found - " ++ file
    Right contents ->
      let lineList = drop 1 (lines contents)  -- Skip header
      in return $ map parseFunc lineList

{-|
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

Parameters:
  fieldName :: String - The name of the field being parsed (used in error message)
  raw       :: String - The raw input string

Return Value:
  Bool - Parsed boolean value

Exceptions:
  Raises a descriptive error if the input is invalid.
-}
parseBoolOrFail :: String -> String -> Bool
parseBoolOrFail fieldName raw =
  case map toUpper (trim raw) of
    "TRUE"  -> True
    "FALSE" -> False
    other   -> error $ "Error: Invalid boolean value " ++ show raw ++
                       " in field '" ++ fieldName ++ "' (expected TRUE or FALSE)"

{-|
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

{-|
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
-}
parseRoom :: String -> Room
parseRoom line =
  let [rid, cap, wca, proj, comp, floor] = parseCSVLine line
      !capacity   = readOrFail "Capacity" cap
      !level      = readOrFail "FloorLevel" floor
      !wheelchair = parseBoolOrFail "WheelchairAccess" wca
      !projector  = parseBoolOrFail "Projector" proj
      !computer   = parseBoolOrFail "Computer" comp
  in Room rid capacity wheelchair projector computer level []

{-|
parseGroup
  Converts a line from the group CSV file into a Group data structure.

Parameters:
  String - CSV line containing GroupID, Size, WheelchairAccess, Projector, Computer, FloorPreference, Start, End

Return Value:
  Group - Parsed Group record

Exceptions:
  Raises a descriptive error on:
    - Invalid integer fields
    - Invalid boolean fields
    - Invalid start or end time
-}
parseGroup :: String -> Group
parseGroup line =
  let [gid, size, wca, proj, comp, floor, start, end] = parseCSVLine line
      !pref       = readFloorPreference floor gid
      !groupSize  = readOrFail "Size" size
      !wheelchair = parseBoolOrFail "WheelchairAccess" wca
      !projector  = parseBoolOrFail "Projector" proj
      !computer   = parseBoolOrFail "Computer" comp
      startTime   = parseDateTime start
      endTime     = parseDateTime end
  in case (startTime, endTime) of
      (Just st, Just et) ->
        Group gid groupSize st et projector computer wheelchair pref
      _ -> error $ "Error: Invalid group entry " ++ gid ++
                   ": Invalid date format in Start or End"

{-|
readFloorPreference
  Parses a floor preference and provides custom validation.

Parameters:
  String - raw floor preference
  String - group ID for context

Return Value:
  Int - valid floor preference or error
-}
readFloorPreference :: String -> String -> Int
readFloorPreference s gid =
  let maybeInt = readMaybe (trim s) :: Maybe Int
  in case maybeInt of
    Just n | n >= -1   -> n
           | otherwise -> error $ "Error: Invalid group entry " ++ gid ++
                                   ": Invalid integer '" ++ s ++
                                   "' in field 'FloorPreference' Expected integer <= -1"
    Nothing -> error $ "Error: Invalid group entry " ++ gid ++
                       ": FloorPreference must be an integer"

{-|
readOrFail
  Attempts to parse a string as an integer or raises a custom error message.

Parameters:
  fieldName :: String - name of the field
  s         :: String - input string

Return Value:
  Int - Parsed integer or triggers error
-}
readOrFail :: String -> String -> Int
readOrFail fieldName s =
  fromMaybe (error $ "Error: Invalid integer in field '" ++ fieldName ++ "'") (readMaybe (trim s))

{-|
runWithGap
  Orchestrates the full execution of the assignment tool: input parsing, solving, and output writing.

Parameters:
  roomsFile  :: FilePath - rooms CSV
  groupsFile :: FilePath - groups CSV
  gap        :: NominalDiffTime - required spacing between assignments in seconds

Behavior:
  - Parses input files
  - Runs the solver
  - Outputs assignments or user-friendly error messages
-}
runWithGap :: FilePath -> FilePath -> NominalDiffTime -> IO ()
runWithGap roomsFile groupsFile gap = do
  roomsResult <- try (loadCSV parseRoom roomsFile) :: IO (Either IOException [Room])
  case roomsResult of
    Left _ -> putStrLn $ "Error: File not found - " ++ roomsFile
    Right rooms -> do
      groupsResult <- try (loadCSV parseGroup groupsFile) :: IO (Either IOException [Group])
      case groupsResult of
        Left _ -> putStrLn $ "Error: File not found - " ++ groupsFile
        Right groups -> case assignGroups groups rooms gap of
          Just sol -> do
            putStrLn "Room Assignments:"
            putStrLn (formatSolution sol)
            writeCSV "assignments.csv" sol
          Nothing -> putStrLn "Error: Constraints cannot be satisfied with the provided input."
