{-|
Module Name: Tests.SolverTests
Project Name: Room Assignment Tool (Functional/Haskell Solution)
File Purpose: Black-box tests for the solver logic using representative inputs
to validate successful assignments, backtracking, and failure scenarios.

Module Summary:
This module verifies that:
- Groups can be assigned when constraints allow (valid case)
- Solver backtracks correctly across multiple rooms (backtracking case)
- Solver fails when constraints cannot be satisfied (invalid case)

Test Type:
Black-box functional tests using HUnit.

Dependencies:
- Solver (assignGroups)
- Tests.Helpers (group, room, testGap)
- HUnit
- Data.Maybe
- Data.List
-}
module Tests.SolverTests (solverTests) where

import Test.HUnit
import Solver
import Tests.Helpers

solverTests :: Test
solverTests = TestList
  [ -- Valid single-room assignment
    TestCase $ do
      let g1 = group "10:00" "11:00" 5 False False False 1 "2023-01-01" "G1"
          g2 = group "11:15" "12:15" 5 False False False 1 "2023-01-01" "G2"
          r  = room "R1" 10 True True True 1
      case assignGroups [g1, g2] [r] testGap of
        Just sol -> assertEqual "Valid groups assigned to one room" 2 (length sol)
        Nothing  -> assertFailure "Expected successful assignment"

    -- Requires backtracking across rooms
  , TestCase $ do
      let g1 = group "10:00" "11:00" 5 False False False 1 "2023-01-01" "G1"
          g2 = group "10:30" "11:30" 5 False False False 1 "2023-01-01" "G2"
          r1 = room "R1" 10 True True True 1
          r2 = room "R2" 10 True True True 1
      case assignGroups [g1, g2] [r1, r2] testGap of
        Just sol -> assertEqual "Backtracking assigns both groups" 2 (length sol)
        Nothing  -> assertFailure "Expected backtracking to succeed"

    -- Unsatisfiable input
  , TestCase $ do
      let g1 = group "10:00" "11:00" 30 False False False 1 "2023-01-01" "G1"
          g2 = group "11:30" "12:30" 30 False False False 1 "2023-01-01" "G2"
          r  = room "R1" 10 True True True 1
      case assignGroups [g1, g2] [r] testGap of
        Just _  -> assertFailure "Expected failure due to capacity"
        Nothing -> return ()  -- Expected
  ]
