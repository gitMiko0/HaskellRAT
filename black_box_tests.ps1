# This script simply runs all the files with the existing executable, along with concise test case headings.
# Set path to your compiled executable
$exePath = "./roomAssign.exe"

# Define all test cases located in ./input/
$tests = @(
    @{Name = "Custom Gap Provided (15 minutes)"; Rooms = "rGap15.csv"; Groups = "gGap15.csv"; Gap = 15},
    @{Name = "Default Gap"; Rooms = "rGapDefault.csv"; Groups = "gGapDefault.csv"},
    @{Name = "Backtracking Case: Successful Large Schedule"; Rooms = "rooms_50.csv"; Groups = "groups_50.csv"},
    @{Name = "Case Sensitivity"; Rooms = "rCaseSens.csv"; Groups = "gCaseSens.csv"},
    @{Name = "No Solution"; Rooms = "rNoSolution.csv"; Groups = "gNoSolution.csv"},
    @{Name = "Invalid Floor Preference"; Rooms = "rooms_50.csv"; Groups = "gInvalidFloorPref.csv"},
    @{Name = "Error: Malformed Booleans"; Rooms = "rooms_50.csv"; Groups = "gInvalidConstraint.csv"},
    @{Name = "Missing Files"; Rooms = "IdontExist.csv"; Groups = "SameHereBro.csv"},
    @{Name = "Error:Year-long Schedule"; Rooms = "rGapDefault.csv"; Groups = "gBadYear.csv"}
    @{Name = "Error:Year-long Schedule"; Rooms = "rGapDefault.csv"; Groups = "gDupeId.csv"}
)

# Run each test from the ./input/ folder and report result
foreach ($test in $tests) {
    Write-Host "`n=== Running Test: $($test.Name) ===" -ForegroundColor Cyan
    try {
        $roomArg = ".\input\$($test.Rooms)"
        $groupArg = ".\input\$($test.Groups)"
        $gapArg = if ($test.ContainsKey("Gap")) { "$($test.Gap)" } else { $null }

        if ($gapArg) {
            & $exePath $roomArg $groupArg $gapArg
        } else {
            & $exePath $roomArg $groupArg
        }
    } catch {
        Write-Host "Test '$($test.Name)' failed with error: $_" -ForegroundColor Red
    }
}
