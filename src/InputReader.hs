{-|
Module Name: InputReader.hs
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Responsible for reading CSV input files and parsing them into Group and Room data structures.

Module Summary:
This module provides functionality to read and parse lines from room and group CSV files using custom parsing rules.
It handles boolean fields, dates, trimming spaces, and gracefully reports format errors for robust preprocessing.
It also ensures that all Room and Group entries are uniquely identified.

Key Functions:
- loadCSV
- parseRoom
- parseGroup
- runWithGap

Dependencies:
- Room.hs (Room data type, getRoomId)
- Group.hs (Group data type, getGroupId)
- OutputWriter.hs (CSV writing)
- Solver.hs (assignGroups)
- Data.Time (for parsing UTCTime)
- Data.Set (for duplicate checking)
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

-- | Loads a CSV file and applies a parsing function to each non-header line.
loadCSV :: (String -> a) -> FilePath -> IO [a]
loadCSV parseFunc file = do
  result <- try (readFile file) :: IO (Either IOException String)
  case result of
    Left _ -> error $ "Error: File not found - " ++ file
    Right contents ->
      let lineList = drop 1 (lines contents)  -- Skip header
      in return $ map parseFunc lineList

-- | Splits a CSV line into individual, trimmed string values.
parseCSVLine :: String -> [String]
parseCSVLine = map trim . splitOn ","

-- | Strictly parses a string into a Bool. Only "TRUE" or "FALSE" (case-insensitive) are accepted.
parseBoolOrFail :: String -> String -> Bool
parseBoolOrFail fieldName raw =
  case map toUpper (trim raw) of
    "TRUE"  -> True
    "FALSE" -> False
    _       -> error $ "Error: Invalid boolean value " ++ show raw ++
                       " in field '" ++ fieldName ++ "' (expected TRUE or FALSE)"

-- | Parses a string in the format "YYYY-MM-DD HH:MM" to UTCTime.
parseDateTime :: String -> Maybe UTCTime
parseDateTime dateStr =
  parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" (trim dateStr)

-- | Removes leading and trailing whitespace from a string.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

-- | Converts a line from the room CSV file into a Room data structure.
parseRoom :: String -> Room
parseRoom line =
  let [rid, cap, wca, proj, comp, floor] = parseCSVLine line
      !capacity   = readOrFail "Capacity" cap
      !level      = readOrFail "FloorLevel" floor
      !wheelchair = parseBoolOrFail "WheelchairAccess" wca
      !projector  = parseBoolOrFail "Projector" proj
      !computer   = parseBoolOrFail "Computer" comp
  in Room rid capacity wheelchair projector computer level []

-- | Converts a line from the group CSV file into a Group data structure.
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
      (Just st, Just et) -> Group gid groupSize st et projector computer wheelchair pref
      _ -> error $ "Error: Invalid group entry " ++ gid ++
                   ": Invalid date format in Start or End"

-- | Parses a floor preference and provides custom validation.
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

-- | Attempts to parse a string as an integer or raises a custom error message.
readOrFail :: String -> String -> Int
readOrFail fieldName s =
  fromMaybe (error $ "Error: Invalid integer in field '" ++ fieldName ++ "'") (readMaybe (trim s))

{-|
runWithGap
  Purpose: Orchestrates the full execution of the assignment tool: input parsing, solving, and output writing.

  Parameters:
    roomsFile  :: FilePath – Path to the rooms CSV file
    groupsFile :: FilePath – Path to the groups CSV file
    gap        :: NominalDiffTime – Required spacing between assignments in seconds

  Return Value:
    IO () – Produces output to console and a CSV file if successful

  Exceptions:
    - Handles missing input files with friendly messages
    - Terminates with descriptive errors if duplicate Group or Room IDs are found
    - Terminates with a message if no valid assignment can be found
-}
runWithGap :: FilePath -> FilePath -> NominalDiffTime -> IO ()
runWithGap roomsFile groupsFile gap = do
  roomsResult <- try (loadCSV parseRoom roomsFile) :: IO (Either IOException [Room])
  case roomsResult of
    Left _ -> putStrLn $ "Error: File not found - " ++ roomsFile
    Right rooms -> do
      checkDuplicates "Room" getRoomID rooms
      groupsResult <- try (loadCSV parseGroup groupsFile) :: IO (Either IOException [Group])
      case groupsResult of
        Left _ -> putStrLn $ "Error: File not found - " ++ groupsFile
        Right groups -> do
          checkDuplicates "Group" getGroupID groups
          case assignGroups groups rooms gap of
            Just sol -> do
              putStrLn "Room Assignments:"
              putStrLn (formatSolution sol)
              writeCSV "assignments.csv" sol
            Nothing -> putStrLn "Error: Constraints cannot be satisfied with the provided input."

{-|
checkDuplicates
  Purpose: Checks for duplicate IDs in a list of elements using a selector function and terminates on conflict.

  Parameters:
    label  :: String – Descriptive name for the item type (e.g., "Group", "Room")
    getId  :: b -> String – Function that extracts the ID from each element
    items  :: [b] – List of elements to check for uniqueness

  Return Value:
    IO () – Passes silently if no duplicates found, terminates with error if duplicates exist

  Exceptions:
    - Raises a descriptive error if duplicate IDs are detected
-}
checkDuplicates :: String -> (b -> String) -> [b] -> IO ()
checkDuplicates label getId items =
  let ids = map getId items
      dups = [x | x <- ids, count x ids > 1]
  in if null dups
     then return ()
     else error $ "Error: Duplicate " ++ label ++ " IDs found: " ++ show (unique dups)

-- | Counts how many times an item appears in the list
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- | Returns the unique elements of a list (first occurrence only)
unique :: Eq a => [a] -> [a]
unique = foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) []
