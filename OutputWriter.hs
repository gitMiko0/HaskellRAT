module OutputWriter where

import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Solver
import System.IO

type Solution = [(String, String, UTCTime, UTCTime)]  -- (GroupID, RoomID, Start, End)

-- Convert a solution into a readable format
formatSolution :: Solution -> String
formatSolution = unlines . map (\(group, room, start, end) -> group ++ " -> " ++ room ++ " : " ++ show start ++ " - " ++ show end)

-- Write assignments to a CSV file
writeCSV :: FilePath -> Solution -> IO ()
writeCSV file solution = do
  writeFile file (unlines (map formatCSVLine solution))

formatCSVLine :: (String, String, UTCTime, UTCTime) -> String
formatCSVLine (group, room, start, end) = group ++ "," ++ room ++ "," ++ show start ++ "," ++ show end
