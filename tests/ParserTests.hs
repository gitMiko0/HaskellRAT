module Tests.ParserTests (parseTests) where

import Test.HUnit
import InputReader
import Room
import Group
import Tests.Helpers
import Control.Exception (evaluate, ErrorCall(..), try)
import Data.Time
import Data.List (isInfixOf)


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
      assertEqual "Start" (parseUTC "2023-01-01 10:00") (getStartTime g)

  , TestCase $ do
      result <- try (evaluate (parseGroup "G1,5,FALSE,FALSE,TRUE,1,invalid-date,2023-01-01 11:00")) :: IO (Either ErrorCall Group)
      case result of
        Left (ErrorCall msg) -> assertBool "Invalid date error" ("Invalid date format" `isInfixOf` msg)
        _ -> assertFailure "Expected error on bad date"

  , TestCase $ do
  result <- try (evaluate (parseRoom "R1,not-a-number,TRUE,TRUE,FALSE,1")) :: IO (Either ErrorCall Room)
  case result of
    Left (ErrorCall msg) -> do
      assertBool "Invalid int error" ("Invalid capacity" `isInfixOf` msg)
    Right _ -> assertFailure "Expected error on bad int"

  ]
