param (
    [Parameter(Mandatory=$true)]
    $day
)
dotnet fsi .\StartNewDay.fsx $Day
