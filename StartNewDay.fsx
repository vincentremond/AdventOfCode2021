#r "nuget: Nustache"

open System.Diagnostics
open Nustache.Core
open System
open System.IO
open System.Text

let template = {| day = DateTime.Today.Day.ToString("00") |} 

let createFolder nameTemplate =
    let name =
        Render.StringToString(nameTemplate, template)

    printfn $"📂 {name}"

    if name |> Directory.Exists |> not then
        Directory.CreateDirectory(name) |> ignore

let addFile (nameTemplate: string) (contentTemplate: string) =
    let name =
        Render.StringToString(nameTemplate, template)

    printfn $"📃 {name}"

    let content =
        Render.StringToString(contentTemplate.TrimStart(), template)

    File.WriteAllText(name, content, new UTF8Encoding(encoderShouldEmitUTF8Identifier = true))

let exec command argumentsTemplate =
    let arguments =
        Render.StringToString(argumentsTemplate, template)

    printfn $"📦 {command} {arguments}"

    Process.Start(command, arguments).WaitForExit()

createFolder "AdventOfCode2021.Day{{{day}}}"

let templateMainProject =
    @"AdventOfCode2021.Day{{{day}}}\AdventOfCode2021.Day{{{day}}}.fsproj"

addFile
    templateMainProject
    "
<Project Sdk=\"Microsoft.NET.Sdk\">
  <PropertyGroup>
    <TargetFramework>net6.0</TargetFramework>
    <GenerateDocumentationFile>true</GenerateDocumentationFile>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    <WarningsAsErrors>25</WarningsAsErrors>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include=\"Solution.fs\" />
  </ItemGroup>
  <ItemGroup>
    <ProjectReference Include=\"..\\AdventOfCode2021.Common\\AdventOfCode2021.Common.fsproj\" />
  </ItemGroup>
</Project>
"

addFile
    @"AdventOfCode2021.Day{{{day}}}\Solution.fs"
    "
namespace AdventOfCode2021.Day{{{day}}}

module Solution =
    let calc inputs = -1

    let part1 = calc
    let part2 = calc
"

createFolder "AdventOfCode2021.Day{{{day}}}.Tests"

let templateTestProject =
    @"AdventOfCode2021.Day{{{day}}}.Tests\AdventOfCode2021.Day{{{day}}}.Tests.fsproj"

addFile
    templateTestProject
    "
<?xml version=\"1.0\" encoding=\"utf-8\"?>
<Project Sdk=\"Microsoft.NET.Sdk\">
  <PropertyGroup>
    <TargetFramework>net6.0</TargetFramework>
    <IsPackable>false</IsPackable>
    <GenerateProgramFile>false</GenerateProgramFile>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    <WarningsAsErrors>25</WarningsAsErrors>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include=\"..\\AdventOfCode2021.Common\\AdventOfCode2021.Common.fsproj\" />
    <ProjectReference Include=\"..\\AdventOfCode2021.Day{{{day}}}\\AdventOfCode2021.Day{{{day}}}.fsproj\" />
  </ItemGroup>
  <ItemGroup>
    <Compile Include=\"UnitTests.fs\" />
    <Content Include=\"inputs.txt\">
      <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
    </Content>
  </ItemGroup>
  <Import Project=\"..\\.paket\\Paket.Restore.targets\" />
</Project>
"

addFile
    @"AdventOfCode2021.Day{{{day}}}.Tests\paket.references"
    "

NUnit
NUnit3TestAdapter
Microsoft.NET.Test.Sdk
Unquote
coverlet.collector
"

addFile
    @"AdventOfCode2021.Day{{{day}}}.Tests\UnitTests.fs"
    "
module AdventOfCode2021.Day{{{day}}}.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day{{{day}}}
open NUnit.Framework
open Swensen.Unquote

let getSample () =
    \"
TODO
\"
    |> String.splitLines
    |> Array.filter String.notNullOrEmpty

let getInputs () = \"inputs.txt\" |> File.ReadAllLines

[<Test>]
let ``1-1 Test part1 with sample`` () = (-1 , (Solution.part1 (getSample ()))) |> Assert.AreEqual
[<Test>]
let ``1-2 Test part1 with inputs`` () = (-1 , (Solution.part1 (getInputs ()))) |> Assert.AreEqual

[<Test>]
let ``2-1 Test part1 with sample`` () = (-1 , (Solution.part2 (getSample ()))) |> Assert.AreEqual
[<Test>]
let ``2-2 Test part1 with inputs`` () = (-1 , (Solution.part2 (getInputs ()))) |> Assert.AreEqual

[<EntryPoint>]
let main _ = 0
"

addFile @"AdventOfCode2021.Day{{{day}}}.Tests\inputs.txt" "TODO"

exec "dotnet" $"sln add \"{templateMainProject}\""
exec "dotnet" $"sln add \"{templateTestProject}\""
