module AdventOfCode2021.Day03.Tests.Part1

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day03
open NUnit.Framework
open FsUnit

[<Test>]
let Test1 () =
    "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
    |> String.splitLines
    |> (Solution.getResultPart1)
    |> should equal 198

[<Test>]
let Test2 () =
    "inputs.txt"
    |> File.ReadAllLines
    |> (Solution.getResultPart2)
    |> should equal 2583164
