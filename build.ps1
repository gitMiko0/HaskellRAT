# build.ps1
ghc -outputdir build -o roomAssign.exe -isrc src/RoomAssignTool.hs
Write-Host "Build complete! Run with .\roomAssign.exe <rooms.csv> <groups.csv>"
