# Functional Solution: Room Assignment Tool (Haskell)

This tool assigns groups into pre-existing rooms while checking for multiple constraints such as:

- **Computer and projector availability**    TRUE or FALSE
- **Wheelchair accessibility**               TRUE or FALSE
- **Room capacity**                          TRUE or FALSE
- **Floor preference**                       (-1 to indicate no preference)
- **Time conflicts**, with a user-defined **time gap** (in minutes) between bookings

The tool ensures that no invalid assignments are made. If a solution exists that satisfies all constraints, it will find it using a backtracking algorithm.  
Otherwise, the tool will communicate that it is not able to find a viable solution.

---

## Input Format

The tool expects two **comma-separated value (.csv)** files as input:

1. `rooms.csv` – contains room properties like capacity, equipment, accessibility, and floor level.  
2. `groups.csv` – contains group scheduling info, size, required equipment, accessibility, and floor preferences.

Sample fields:
- **Room file:** `RoomID,Capacity,WheelchairAccess,Projector,Computer,FloorLevel`  
- **Group file:** `GroupID,Size,WheelchairAccess,Projector,Computer,FloorPreference,Start,End`  

Sample inputs are provided in the input folder (consult groups_50.csv and/or rooms_50.csv as valid samples)

IDs MUST be UNIQUE

3. Optional: time gap (in minutes)

---

## How to run the tool?

From the terminal, run:

```bash
ghc -outputdir build -o roomAssign.exe -isrc src/RoomAssignTool.hs
```
Then, an executable will be created, run:
```bash
./roomAssign.exe <rooms_file.csv> <groups_file.csv> <optional_gap_in_minutes>
```

Note that there are also powershell scripts available.
to build the executable, input:
```bash
./build.ps1
```
to run the tests, input:
```bash
./black_box_tests.ps1
```

Note: The testing also has comprehensive, implementation-based, function specific tests with HUnit along with their coverage frameworks under the tests folder. Please ignore this.
