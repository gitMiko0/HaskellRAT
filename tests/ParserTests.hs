{-
Module Name: Tests.ParserTests
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Unit tests for input parsing from CSV strings into Room and Group data.

Module Summary:
This module verifies that:
- Well-formed CSV strings are correctly parsed into Room and Group records
- Invalid date and integer values raise appropriate errors
- Invalid boolean values are also detected and reported
- The loadCSV function correctly reads and parses files into lists of data

Test Type:
Black-box unit tests using HUnit and exception handling via Control.Exception.

Dependencies:
- InputReader (for parseRoom, parseGroup, loadCSV, runWithGap)
- Room and Group data types
- HUnit, Control.Exception
- System.IO
- Data.Time.Clock
- System.IO.Silently
-}
module Tests.ParserTests (parseTests) where

import Test.HUnit
import InputReader
import Room
import Group
import Control.Exception (evaluate, ErrorCall(..), try)
import Data.Time
import Data.List (isInfixOf)
import Data.Time.Clock (secondsToNominalDiffTime)



parseTests :: Test
parseTests = TestList
  [ TestCase $ do
      let input = "R1,10,TRUE,FALSE,TRUE,2"
          r = parseRoom input
      assertEqual "Room ID" "R1" (getRoomID r)

  , TestCase $ do
      let input = "G1,5,FALSE,FALSE,TRUE,1,2023-01-01 10:00,2023-01-01 11:00"
          g = parseGroup input
      assertEqual "Group ID" "G1" (getGroupID g)
      assertEqual "Start" (getStartTime g) (getStartTime (parseGroup input))

  , TestCase $ do
      result <- try (evaluate (parseGroup "G1,5,FALSE,FALSE,TRUE,1,invalid-date,2023-01-01 11:00")) :: IO (Either ErrorCall Group)
      case result of
        Left (ErrorCall msg) -> assertBool "Invalid date error" ("Invalid date format" `isInfixOf` msg)
        _ -> assertFailure "Expected error on bad date"

  , TestCase $ do
      result <- try (evaluate (parseRoom "R1,not-a-number,TRUE,TRUE,FALSE,1")) :: IO (Either ErrorCall Room)
      case result of
        Left (ErrorCall msg) -> assertBool "Invalid int error" ("Invalid capacity" `isInfixOf` msg)
        _ -> assertFailure "Expected error on bad int"

  , TestCase $ do
      result <- try (evaluate (parseRoom "R1,10,maybe,TRUE,FALSE,1")) :: IO (Either ErrorCall Room)
      case result of
        Left (ErrorCall msg) -> assertBool "Invalid bool error" ("Invalid boolean for WheelchairAccess" `isInfixOf` msg)
        _ -> assertFailure "Expected error on bad boolean"

    -- The following tests ensure that the loading function is correctly populating the data structures
  , TestCase $ do
      rooms <- loadCSV parseRoom "Tests/test_rooms.csv"
      let expected =
            [ Room "R101" 25 True False False 1 []
            , Room "R102" 30 False True True 2 []
            ]
      assertEqual "loadCSV parses valid rooms" expected rooms

  , TestCase $ do
      groups <- loadCSV parseGroup "Tests/test_groups.csv"
      let expected =
        -- using Prelude read for simplicity of testing
            [ Group "G0001" 40 (read "2025-02-07 08:00:00 UTC") (read "2025-02-07 09:00:00 UTC") True False True (-1)
            , Group "G0002" 35 (read "2025-02-07 10:00:00 UTC") (read "2025-02-07 11:00:00 UTC") True False False (-1)
            ]

      assertEqual "loadCSV parses valid groups" expected groups
  ]