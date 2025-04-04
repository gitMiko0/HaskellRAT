import Test.HUnit
import Data.Time
import InputReader
import Solver
import OutputWriter
import System.Environment (getArgs)
import Room
import Group
import Data.Time.Format (defaultTimeLocale, parseTimeM)

-- Helper Functions
group :: String -> String -> Int -> Bool -> Bool -> Bool -> Int -> String -> String -> Group
group start end size wheelchair projector computer floor dateStr groupId =
    let date = parseDate dateStr
        startTime = parseTimeOfDay start
        endTime = parseTimeOfDay end
    in Group groupId size (dateTime date startTime) (dateTime date endTime) projector computer wheelchair floor


room :: String -> Int -> Bool -> Bool -> Bool -> Int -> Room
room roomId capacity wheelchair projector computer floor =
    Room roomId capacity wheelchair projector computer floor []

parseUTC :: String -> UTCTime
parseUTC s = parseTimeOrError True defaultTimeLocale "%F %R" s

dateTime :: Day -> TimeOfDay -> UTCTime
dateTime date time = UTCTime date (timeOfDayToTime time)

parseDate :: String -> Day
parseDate s = case parseTimeM True defaultTimeLocale "%F" s of
    Just d -> d
    Nothing -> error $ "Invalid date format: " ++ s

parseTimeOfDay :: String -> TimeOfDay
parseTimeOfDay s = case parseTimeM True defaultTimeLocale "%R" s of
    Just t -> t
    Nothing -> error $ "Invalid time format: " ++ s

-- Tests
testCheckFloorPreference = TestList [
    "Any floor preference" ~: checkFloorPreference (group "10:00" "11:00" 5 False False False (-1) "2023-01-01" "G1") (room "R1" 10 True True True 2) ~?= True,
    "Matching floor preference" ~: checkFloorPreference (group "10:00" "11:00" 5 False False False 2 "2023-01-01" "G1") (room "R1" 10 True True True 2) ~?= True,
    "Non-matching floor preference" ~: checkFloorPreference (group "10:00" "11:00" 5 False False False 3 "2023-01-01" "G1") (room "R1" 10 True True True 2) ~?= False
    ]

testCheckRoomCapacity = TestList [
    "Exact fit" ~: checkRoomCapacity (group "10:00" "11:00" 10 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True,
    "Over capacity" ~: checkRoomCapacity (group "10:00" "11:00" 11 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= False,
    "Under capacity" ~: checkRoomCapacity (group "10:00" "11:00" 9 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True
    ]

testCheckWheelchairAccess = TestList [
    "Accessible room" ~: checkWheelchairAccess (group "10:00" "11:00" 5 True False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True,
    "Non-accessible room" ~: checkWheelchairAccess (group "10:00" "11:00" 5 True False False 1 "2023-01-01" "G1") (room "R1" 10 False True True 1) ~?= False
    ]

testCheckEquipment = TestList [
    "Matching equipment" ~: checkEquipment (group "10:00" "11:00" 5 False True True 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True,
    "Mismatched equipment" ~: checkEquipment (group "10:00" "11:00" 5 False True False 1 "2023-01-01" "G1") (room "R1" 10 True False True 1) ~?= False
    ]

testIsValidAssignment = TestList [
    "Valid assignment" ~: isValidAssignment (group "10:00" "11:00" 5 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True,
    "Invalid due to capacity" ~: isValidAssignment (group "10:00" "11:00" 15 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= False
    ]

testMissingEquip = TestList [
    "Valid assignment" ~: isValidAssignment (group "10:00" "11:00" 5 False False False 1 "2023-01-01" "G1") (room "R1" 10 True True True 1) ~?= True,
    "Missing Equipments" ~: isValidAssignment (group "10:00" "11:00" 15 True True True 1 "2023-01-01" "G1") (room "R1" 10 False False False 1) ~?= False
    ]

testInvalidGroup = TestList [
    "Invalid group" ~: assignRooms [(group "10:00" "11:00" 15 True True True 1 "2023-01-01" "G1")] [(room "R1" 10 False False False 1)] ~?= []
    ]

testValidGroup = TestList [
    let expectedStart = parseUTC "2023-01-01 10:00"
        expectedEnd = parseUTC "2023-01-01 11:00"
    in "Valid group" ~: assignRooms 
        [group "10:00" "11:00" 5 True True True 1 "2023-01-01" "G1"]
        [room "R1" 10 True True True 1]
        ~?= [[("G1", "R1", expectedStart, expectedEnd)]]
    ]

main :: IO ()
main = runTestTTAndExit $ TestList [
    testCheckFloorPreference,
    testCheckRoomCapacity,
    testCheckWheelchairAccess,
    testCheckEquipment,
    testIsValidAssignment,
    testMissingEquip,
    testInvalidGroup,
    testValidGroup
    ]
