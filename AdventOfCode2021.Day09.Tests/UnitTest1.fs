module AdventOfCode2021.Day09.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day09
open NUnit.Framework
open Swensen.Unquote

let getSample () =
    "
2199943210
3987894921
9856789892
8767896789
9899965678
"
    |> String.splitLines
    |> Array.filter String.notNullOrEmpty

let getInputs () = "inputs.txt" |> File.ReadAllLines

let genericTest sample calc expectedResult = test <@ calc sample = expectedResult @>

[<Test>]
let ``1-1 Test part1 with sample`` () =
    genericTest (getSample ()) Solution.part1 15

[<Test>]
let ``1-2 Test part1 with inputs`` () =
    genericTest (getInputs ()) Solution.part1 600

[<Test>]
let ``2-1 Test part1 with sample`` () =
    genericTest (getSample ()) Part2.part2 1134

[<Test>]
let ``2-2 Test part1 with inputs`` () =
    genericTest (getInputs ()) Part2.part2 987840

[<EntryPoint>]
let main _ = 0
