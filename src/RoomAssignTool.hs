{- 
Module Name: RoomAssignTool.hs
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Acts as the program entry point, handling command-line arguments,
             including an optional time gap in minutes.

Module Summary:
This module:
- Reads CLI args
- Converts time gap from minutes to seconds
- Delegates loading, solving, and output to runWithGap in InputReader.hs

Key Functions:
- `main`

Dependencies:
- InputReader.hs (loaders, parser, time gap handler, runWithGap)

Notes:
- Time gap defaults to 10 minutes if invalid.
- Only the first valid solution is displayed.
-}

import InputReader (loadCSV, parseRoom, parseGroup, runWithGap)
import Solver (assignRooms)
import OutputWriter (formatSolution, writeCSV)
import System.Environment (getArgs)
import Data.Time (NominalDiffTime)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [roomsFile, groupsFile] ->
      runWithGap roomsFile groupsFile 600  -- default to 10 minutes
    [roomsFile, groupsFile, gapStr] ->
      case readMaybe gapStr :: Maybe Int of
        Just gapMin -> runWithGap roomsFile groupsFile (fromIntegral (gapMin * 60))
        Nothing     -> putStrLn "Error: Gap must be an integer in minutes."
    _ -> putStrLn "Usage: room_assign_tool <rooms.csv> <groups.csv> [gap_minutes]"



