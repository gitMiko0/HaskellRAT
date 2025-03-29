module Main where

import InputReader
import Solver
import OutputWriter
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [roomsFile, groupsFile] -> do
      rooms <- loadCSV parseRoom roomsFile  
      groups <- loadCSV parseGroup groupsFile
      let solutions = assignRooms groups rooms
      case solutions of
        (sol:_) -> do
          putStrLn "Room Assignments:"
          putStrLn (formatSolution sol)
          writeCSV "assignments.csv" sol
        [] -> putStrLn "Error: Constraints cannot be satisfied with the provided input."
    _ -> putStrLn "Usage: room_assign_tool <rooms.csv> <groups.csv>"

