module Tests.ConstraintTests (constraintTests) where

import Test.HUnit
import Room
import Group
import Constraints
import Tests.Helpers

constraintTests :: Test
constraintTests = TestList
  [ testFloorPreference
  , testCapacity
  , testWheelchair
  , testEquipment
  , testFullAssignmentPass
  , testFullAssignmentFailEach
  ]

-- Floor preference: -1 = no preference, match = ok, mismatch = fail
testFloorPreference :: Test
testFloorPreference = TestList
  [ "No preference (-1)" ~: isValidAssignment gNoPref r2 testGap ~?= True
  , "Preferred floor match" ~: isValidAssignment gPref2 r2 testGap ~?= True
  , "Preferred floor mismatch" ~: isValidAssignment gPref3 r2 testGap ~?= False
  ]
  where
    gNoPref = group "10:00" "11:00" 5 False False False (-1) "2023-01-01" "G1"
    gPref2  = group "10:00" "11:00" 5 False False False 2 "2023-01-01" "G2"
    gPref3  = group "10:00" "11:00" 5 False False False 3 "2023-01-01" "G3"
    r2      = room "R2" 10 True True True 2

-- Room capacity edge cases
testCapacity :: Test
testCapacity = TestList
  [ "Under capacity" ~: isValidAssignment gSmall rCap10 testGap ~?= True
  , "Exact capacity" ~: isValidAssignment gExact rCap10 testGap ~?= True
  , "Over capacity" ~: isValidAssignment gLarge rCap10 testGap ~?= False
  ]
  where
    rCap10 = room "R1" 10 True True True 1
    gSmall = group "10:00" "11:00" 9 False False False 1 "2023-01-01" "G1"
    gExact = group "10:00" "11:00" 10 False False False 1 "2023-01-01" "G2"
    gLarge = group "10:00" "11:00" 11 False False False 1 "2023-01-01" "G3"

-- Wheelchair access
testWheelchair :: Test
testWheelchair = TestList
  [ "Does not need wheelchair" ~: isValidAssignment gNoNeed rNoWheel testGap ~?= True
  , "Needs wheelchair, room has it" ~: isValidAssignment gNeed rWheel testGap ~?= True
  , "Needs wheelchair, room lacks it" ~: isValidAssignment gNeed rNoWheel testGap ~?= False
  ]
  where
    gNoNeed   = group "10:00" "11:00" 5 False False False 1 "2023-01-01" "G1"
    gNeed     = group "10:00" "11:00" 5 True False False 1 "2023-01-01" "G2"
    rWheel    = room "R1" 10 True False False 1
    rNoWheel  = room "R2" 10 False False False 1

-- Equipment (projector, computer)
testEquipment :: Test
testEquipment = TestList
  [ "No equipment needed" ~: isValidAssignment gNone rNone testGap ~?= True
  , "Needs projector only" ~: isValidAssignment gProj rProj testGap ~?= True
  , "Needs projector missing" ~: isValidAssignment gProj rNone testGap ~?= False
  , "Needs both, room has both" ~: isValidAssignment gBoth rBoth testGap ~?= True
  , "Needs both, room lacks one" ~: isValidAssignment gBoth rProj testGap ~?= False
  ]
  where
    gNone  = group "10:00" "11:00" 5 False False False 1 "2023-01-01" "G1"
    gProj  = group "10:00" "11:00" 5 False True False 1 "2023-01-01" "G2"
    gBoth  = group "10:00" "11:00" 5 False True True 1 "2023-01-01" "G3"
    rNone  = room "R1" 10 True False False 1
    rProj  = room "R2" 10 True True False 1
    rBoth  = room "R3" 10 True True True 1

-- End-to-end pass
testFullAssignmentPass :: Test
testFullAssignmentPass = "All constraints satisfied" ~:
  isValidAssignment g r testGap ~?= True
  where
    g = group "10:00" "11:00" 5 True True True 2 "2023-01-01" "G1"
    r = room "R1" 5 True True True 2

-- Fail each constraint (one by one)
testFullAssignmentFailEach :: Test
testFullAssignmentFailEach = TestList
  [ "Fail: floor mismatch" ~: isValidAssignment g rWrongFloor testGap ~?= False
  , "Fail: capacity"       ~: isValidAssignment gTooBig rOK testGap ~?= False
  , "Fail: wheelchair"     ~: isValidAssignment gWheel rNoWheel testGap ~?= False
  , "Fail: projector"      ~: isValidAssignment gProj rNoProj testGap ~?= False
  , "Fail: computer"       ~: isValidAssignment gComp rNoComp testGap ~?= False
  ]
  where
    g        = group "10:00" "11:00" 5 True True True 2 "2023-01-01" "G1"
    gTooBig  = group "10:00" "11:00" 15 False False False 1 "2023-01-01" "G2"
    gWheel   = group "10:00" "11:00" 5 True False False 1 "2023-01-01" "G3"
    gProj    = group "10:00" "11:00" 5 False True False 1 "2023-01-01" "G4"
    gComp    = group "10:00" "11:00" 5 False False True 1 "2023-01-01" "G5"

    rOK         = room "R1" 10 True True True 2
    rWrongFloor = room "R2" 10 True True True 1
    rNoWheel    = room "R3" 10 False True True 1
    rNoProj     = room "R4" 10 True False True 1
    rNoComp     = room "R5" 10 True True False 1
