# Room Assignment Tool Coverage Report Script

# Config
$TestFile = "Tests/Tests.hs"
$ExeName = "test-suite"
$TixFile = "$ExeName.exe.tix"
$CoverageDir = "coverage"

Write-Host "Cleaning previous build and coverage artifacts..."
Remove-Item .\*.tix -Force -ErrorAction SilentlyContinue
Remove-Item -Recurse -Force .hpc, $CoverageDir, build -ErrorAction SilentlyContinue
Remove-Item .\*.exe -Force -ErrorAction SilentlyContinue
Get-ChildItem -Path ., ./src, ./Tests -Include *.hi, *.o -Recurse -Force -ErrorAction SilentlyContinue | Remove-Item -Force

Write-Host "Compiling test suite with coverage enabled..."
ghc -fhpc -outputdir build -iTests -isrc $TestFile -o $ExeName

Write-Host "Running test suite..."
Start-Process -Wait -FilePath ".\$ExeName.exe"

if (Test-Path $TixFile) {
    Write-Host "Generating HTML coverage report..."
    hpc markup $TixFile --destdir=$CoverageDir

    $reportPath = "$CoverageDir\hpc_index.html"
    if (Test-Path $reportPath) {
        Write-Host "Opening coverage report in browser..."
        Start-Process $reportPath
    } else {
        Write-Host "Coverage report not found."
    }
} else {
    Write-Host "No .tix file found. The test suite may have failed to run properly."
}
