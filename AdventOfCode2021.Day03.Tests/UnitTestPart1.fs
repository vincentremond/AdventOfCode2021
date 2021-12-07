module AdventOfCode2021.Day03.Tests.Part1

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day03
open NUnit.Framework
open Swensen.Unquote

[<Test>]
let Test1 () =
    test
        <@ "00100
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
           |> (Solution.getResultPart1) = 198 @>

[<Test>]
let Test2 () =
    test
        <@ "inputs.txt"
           |> File.ReadAllLines
           |> (Solution.getResultPart2) = 2784375 @>
