module OutputWriter (
  formatSolution,
  writeCSV
) where

import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import System.IO
import Solver (Solution)

-- | Converts a list of assignments (Solution) into a human-readable multiline string.
--   Format: "GroupID -> RoomID : YYYY-MM-DD HH:MM - YYYY-MM-DD HH:MM"
formatSolution :: Solution -> String
formatSolution = unlines . map formatLine
  where
    fmt t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" t
    formatLine (group, room, start, end) =
      group ++ " -> " ++ room ++ " : " ++ fmt start ++ " - " ++ fmt end

-- | Writes a solution to a CSV file.
--   Format per line: GroupID,RoomID,StartTime,EndTime
writeCSV :: FilePath -> Solution -> IO ()
writeCSV file solution = do
  writeFile file (unlines (map formatAsCSV solution))

-- | Formats a single assignment as a CSV line.
formatAsCSV :: (String, String, UTCTime, UTCTime) -> String
formatAsCSV (group, room, start, end) =
  let fmt = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
  in group ++ "," ++ room ++ "," ++ fmt start ++ "," ++ fmt end
