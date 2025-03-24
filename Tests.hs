import Test.HUnit
import Data.Time
import Solver (assignGroups, isValidAssignment, checkFloorPreference, checkRoomCapacity,
               checkWheelchairAccess, checkEquipment, checkTimeOverlap, preprocessData)

-- Sample Group and Room Data Using Your Structure
group :: String -> String -> Int -> Bool -> Bool -> Bool -> Int -> String -> String -> Group
group start end size wheelchair projector computer floor dateStr groupId =
    let date = parseDate dateStr
        startTime = parseTimeOfDay start
        endTime = parseTimeOfDay end
    in Group groupId (dateTime date startTime) (dateTime date endTime) size wheelchair projector computer floor

room :: String -> Int -> Bool -> Bool -> Bool -> Int -> Room
room roomId capacity wheelchair projector computer floor =
    Room roomId capacity wheelchair projector computer floor []

-- Utility Functions
parseDate :: String -> Day
parseDate = read

parseTimeOfDay :: String -> TimeOfDay
parseTimeOfDay = read

dateTime :: Day -> TimeOfDay -> UTCTime
dateTime date time = UTCTime date (timeOfDayToTime time)

-- Unit Tests
testCheckFloorPreference :: Test
testCheckFloorPreference = TestList [
    "Any floor preference" ~: checkFloorPreference (group "10:00" "11:00" 5 False False False (-1) "2023-01-01" "G1") (room "R1" 10 True True True 2) ~?= True,
    "Matching floor preference" ~: checkFloorPreference (group "10:00" "11:00" 5 False False False 2 "2023-01-01" "G1") (room "R1" 10 True True True 2) ~?= True,
    "Non-matching floor preference" ~: checkFloorPreference (group "10:00" "11:00" 5 False False False 3 "2023-01-01" "G1") (room "R1" 10 True True True 2) ~?= False
    ]

testCheckRoomCapacity :: Test
testCheckRoomCapacity = TestList [
    "Exact fit" ~: checkRoomCapacity (group "10:00" "11:00" 10 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True,
    "Over capacity" ~: checkRoomCapacity (group "10:00" "11:00" 11 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= False,
    "Under capacity" ~: checkRoomCapacity (group "10:00" "11:00" 9 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True
    ]

testCheckWheelchairAccess :: Test
testCheckWheelchairAccess = TestList [
    "Accessible room" ~: checkWheelchairAccess (group "10:00" "11:00" 5 True False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True,
    "Non-accessible room" ~: checkWheelchairAccess (group "10:00" "11:00" 5 True False False 1 "2023-01-01" "G1") (room "R1" 10 False True True 1) ~?= False
    ]

testCheckEquipment :: Test
testCheckEquipment = TestList [
    "Matching equipment" ~: checkEquipment (group "10:00" "11:00" 5 False True True 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True,
    "Mismatched equipment" ~: checkEquipment (group "10:00" "11:00" 5 False True False 1 "2023-01-01" "G1") (room "R1" 10 True False True 1) ~?= False
    ]

testIsValidAssignment :: Test
testIsValidAssignment = TestList [
    "Valid assignment" ~: isValidAssignment (group "10:00" "11:00" 5 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True,
    "Invalid due to capacity" ~: isValidAssignment (group "10:00" "11:00" 15 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= False
    ]

main :: IO ()
main = runTestTTAndExit $ TestList [
    testCheckFloorPreference,
    testCheckRoomCapacity,
    testCheckWheelchairAccess,
    testCheckEquipment,
    testIsValidAssignment
    ]
