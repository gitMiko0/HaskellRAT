module Tests.SolverTests (solverTests) where

import Test.HUnit
import Solver
import Tests.Helpers

solverTests :: Test
solverTests = TestList
  [ TestCase $ do
      let g1 = group "10:00" "11:00" 5 False False False 1 "2023-01-01" "G1"
          g2 = group "11:10" "12:10" 5 False False False 1 "2023-01-01" "G2"
          r  = room "R1" 10 True True True 1
      case assignRooms [g1, g2] [r] testGap of
        Just sol -> assertEqual "Two groups should be scheduled" 2 (length sol)
        Nothing -> assertFailure "Expected successful assignment"

  , TestCase $ do
      let g1 = group "10:00" "11:00" 5 False False False 1 "2023-01-01" "G1"
          g2 = group "10:05" "11:05" 5 False False False 1 "2023-01-01" "G2"
          r  = room "R1" 10 True True True 1
      assertEqual "Groups should conflict and fail" Nothing (assignRooms [g1, g2] [r] testGap)
  ]
