# build.ps1
ghc -o roomAssign RoomAssignTool.hs
Write-Host "Build complete! Run with .\roomAssign.exe <rooms.csv> <groups.csv>"
