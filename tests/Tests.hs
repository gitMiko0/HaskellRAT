module Main where

import Test.HUnit
import Tests.ConstraintTests (constraintTests)
import Tests.SolverTests (solverTests)
import Tests.ParserTests (parseTests)

main :: IO ()
main = runTestTTAndExit $ TestList
  [ constraintTests
  , solverTests
  , parseTests
  ]
