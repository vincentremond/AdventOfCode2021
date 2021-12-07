module AdventOfCode2021.Day05.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day05
open NUnit.Framework
open Swensen.Unquote

[<Test>]
let Test1 () =

    let sample =
        "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"
        |> String.splitLines
        |> Array.toList

    let inputs =
        "inputs.txt" |> File.ReadAllLines |> Array.toList

    [ sample, Solution.part1, 5
      sample, Solution.part2, 12

      inputs, Solution.part1, 8622
      inputs, Solution.part2, 22037 ]
    |> Seq.iter (fun (data, f, result) -> test <@ (f data) = result @>)
