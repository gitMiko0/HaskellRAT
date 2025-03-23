module OutputWriter where

import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Solver
import System.IO

-- Convert a solution into a readable format
formatSolution :: Solution -> String
formatSolution = unlines . map (\(g, r, s, e) -> g ++ " -> " ++ r ++ " : " ++ show s ++ " - " ++ show e)

-- Write assignments to a CSV file
writeCSV :: FilePath -> Solution -> IO ()
writeCSV file solution = do
  writeFile file (unlines (map formatCSVLine solution))

formatCSVLine :: (String, String, UTCTime, UTCTime) -> String
formatCSVLine (g, r, s, e) = g ++ "," ++ r ++ "," ++ show s ++ "," ++ show e
