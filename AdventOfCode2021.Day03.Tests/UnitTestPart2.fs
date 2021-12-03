module AdventOfCode2021.Day03.Tests.Part2

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day03
open NUnit.Framework
open FsUnit

[<Test>]
let Test1 () =

    let sample =
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

    let inputs = "inputs.txt" |> File.ReadAllLines

    [
        sample, Solution.getResultPart1, 198
        inputs, Solution.getResultPart1, 2583164
        sample, Solution.getResultPart2, 230
        inputs, Solution.getResultPart2, 2784375
    ]
    |> Seq.iter (fun (data, f, result) -> f data |> should equal result)
