module AdventOfCode2021.Day01.Tests.Part1

open System.IO
open AdventOfCode2021.Day01
open NUnit.Framework
open FsUnit

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () =
    Solution.calc [| 199
                     200
                     208
                     210
                     200
                     207
                     240
                     269
                     260
                     263 |]
    |> should equal 7

[<Test>]
let Test2 () =
    "UnitTestPart1.inputs.txt"
    |> File.ReadAllLines
    |> Seq.map int
    |> Solution.calc
    |> should equal 1709
