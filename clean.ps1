# Clean Haskell intermediate files in root, src/, and Tests/
Get-ChildItem -Path ., ./src, ./Tests -Include *.hi, *.o -Recurse -Force -ErrorAction SilentlyContinue | Remove-Item -Force

# Remove output executable(s)
Remove-Item .\*.exe -Force -ErrorAction SilentlyContinue

# Remove coverage and build directories
Remove-Item -Recurse -Force .hpc, coverage, build -ErrorAction SilentlyContinue

# Remove .tix files
Remove-Item .\*.tix -Force -ErrorAction SilentlyContinue

Write-Host "Cleanup complete!"
