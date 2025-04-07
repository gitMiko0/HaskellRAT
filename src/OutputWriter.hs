{-
Module Name: OutputWriter.hs
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Responsible for formatting and exporting the final assignment solution to human-readable output or CSV.

Module Summary:
This module provides functionality to format a solution (list of assignments) either as a multiline string for console
display, or as a CSV file suitable for output submission. It uses standard time formatting for readability.

Key Functions:
- `formatSolution` - generates a readable string version of the assignments
- `writeCSV` - writes assignments to a CSV file
- `formatAsCSV` - formats a single assignment into a CSV row

Dependencies:
- Data.Time (for formatting timestamps)
- System.IO (for file output)
- Solver.hs (uses the `Solution` type alias)

Known/Suspected Errors:
- None known at this time
-}

module OutputWriter (
  formatSolution,
  writeCSV
) where

import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.List (intercalate)

import System.IO
import Solver (Solution)

{-
formatSolution
  Converts a list of assignments into a multiline string with a readable format.

Parameters:
  Solution - A list of tuples (GroupID, RoomID, StartTime, EndTime)

Return Value:
  String - Formatted string with each assignment on its own line
-}
formatSolution :: Solution -> String
formatSolution = unlines . map formatLine
  where
    fmt t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" t
    formatLine (group, room, start, end) =
      group ++ " -> " ++ room ++ " : " ++ fmt start ++ " - " ++ fmt end

{-|
writeCSV
  Writes the formatted solution as a CSV file with a header line.

Parameters:
  FilePath - the target CSV file path
  Solution - the list of group-room-time assignments

Return Value:
  IO () - performs the write side-effect

Exceptions:
  May raise I/O exceptions if the file path is invalid or unwritable.
-}
writeCSV :: FilePath -> Solution -> IO ()
writeCSV path sol = writeFile path $ unlines $
  ["GroupID,RoomID,Start,End"] ++ map toCSV sol --header
  where
    toCSV (gid, rid, start, end) =
      intercalate "," [gid, rid, fmt start, fmt end]  --join separated with commas
    fmt = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

{-
formatAsCSV
  Formats a single assignment tuple as a CSV line.

Parameters:
  (String, String, UTCTime, UTCTime) - (GroupID, RoomID, Start, End)

Return Value:
  String - Comma-separated string suitable for CSV export
-}
formatAsCSV :: (String, String, UTCTime, UTCTime) -> String
formatAsCSV (group, room, start, end) =
  let fmt = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
  in group ++ "," ++ room ++ "," ++ fmt start ++ "," ++ fmt end
