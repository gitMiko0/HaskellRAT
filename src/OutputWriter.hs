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

{-
writeCSV
  Writes the solution to a CSV file with the format:
  GroupID,RoomID,StartTime,EndTime

Parameters:
  file :: FilePath - The output CSV file path
  solution :: Solution - The list of assignments to export

Return Value:
  IO () - Performs file writing as a side effect
-}
writeCSV :: FilePath -> Solution -> IO ()
writeCSV file solution = do
  writeFile file (unlines (map formatAsCSV solution))

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
