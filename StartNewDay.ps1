$Day = (Get-Date).Day.ToString("00")

$LibProject = "AdventOfCode2021.Day$($Day)"
$TstProject = "AdventOfCode2021.Day$($Day).Tests"

$CommonProject = (Get-ChildItem "AdventOfCode2021.Common.fsproj" -Force -Recurse | Select-Object -First 1).FullName

function Exec($cmd) {
    Write-Host "🔷 $cmd" -ForegroundColor Yellow
    Invoke-Expression $cmd | Out-String | Write-Host
}

function NewProj ($proj, $type) {
    $fproj = "$proj\$proj.fsproj"
    Exec "New-Item $proj -ItemType Directory"
    Exec "Set-Location $proj"
    Exec "dotnet new $type --language F#"
    Exec "Set-Location .."
    Exec "dotnet sln add ""$fproj"""
    return $fproj
}

$LibProjectFile = NewProj "$($LibProject)" "classlib"
$TstProjectFile = NewProj "$($TstProject)" "nunit"

Exec "dotnet add $LibProjectFile reference $CommonProject"

Exec "dotnet add $TstProjectFile reference $LibProjectFile"
Exec "dotnet add $TstProjectFile reference $CommonProject"

foreach ($pkg in @("NUnit", "NUnit3TestAdapter", "Microsoft.NET.Test.Sdk", "Unquote", "coverlet.collector")) {
    Exec "dotnet remove $TstProjectFile package $pkg"
    Exec "dotnet paket add $pkg --project $TstProjectFile"
}

# TODO alchata

"" | Out-File "$LibProject\inputs.txt"
# dotnet alchata fsproj --Include "inputs.txt" --Type Content --CopyToOutputDirectory PreserveNewest --Position -1
# dotnet alchata fsproj --Exclude "Program.fs"
