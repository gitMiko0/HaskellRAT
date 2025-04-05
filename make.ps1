# make.ps1

# Clean old files
Remove-Item *.hi, *.o, *.exe -Force -ErrorAction SilentlyContinue

# Compile using GHC
ghc -o roomAssign RoomAssignTool.hs

# Clean old files
Remove-Item *.hi, *.o, *.exe -Force -ErrorAction SilentlyContinue