module AdventOfCode2021.Day01.Tests.Part2

open System.IO
open AdventOfCode2021.Day01
open NUnit.Framework
open FsUnit

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () =
    Solution.calcPart2 [| 199
                          200
                          208
                          210
                          200
                          207
                          240
                          269
                          260
                          263 |]
    |> should equal 5

[<Test>]
let Test2 () =
    "UnitTestPart2.inputs.txt"
    |> File.ReadAllLines
    |> Seq.map int
    |> Solution.calcPart2
    |> should equal 1761
