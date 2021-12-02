$Day = (Get-Date).Day.ToString("00")

$LibProject = "AdventOfCode2021.Day$($Day)"
$TstProject = "AdventOfCode2021.Day$($Day).Tests"

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

Exec "dotnet add $TstProjectFile reference $LibProjectFile"

foreach ($pkg in @("NUnit", "NUnit3TestAdapter", "Microsoft.NET.Test.Sdk", "FsUnit", "coverlet.collector")) {
    Exec "dotnet remove $TstProjectFile package $pkg"
    Exec "dotnet paket add $pkg --project $TstProjectFile"
}
