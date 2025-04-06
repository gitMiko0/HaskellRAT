# make.ps1

# Clean old files
Remove-Item *.hi, *.o, *.exe -Force -ErrorAction SilentlyContinue

# Compile using GHC
ghc -o roomAssign RoomAssignTool.hs

# Clean old files
Remove-Item *.hi, *.o, *.exe -Force -ErrorAction SilentlyContinue
Write-Host "Build complete! Run with .\roomAssign.exe <rooms.csv> <groups.csv>"