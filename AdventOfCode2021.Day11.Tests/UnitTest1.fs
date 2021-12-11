module AdventOfCode2021.Day11.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day11
open NUnit.Framework
open Swensen.Unquote

let getSample () =
    "
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
"
    |> String.splitLines
    |> Array.filter String.notNullOrEmpty

let getInputs () = "inputs.txt" |> File.ReadAllLines

let genericTest sample calc expectedResult =
    test <@ calc sample = expectedResult @>

[<Test>]
let ``1-1 Test part1 with sample`` () =
    genericTest (getSample ()) Solution.part1 1656

[<Test>]
let ``1-2 Test part1 with inputs`` () =
    genericTest (getInputs ()) Solution.part1 1700

[<Test>]
let ``2-1 Test part1 with sample`` () =
    genericTest (getSample ()) Solution.part2 195

[<Test>]
let ``2-2 Test part1 with inputs`` () =
    genericTest (getInputs ()) Solution.part2 273
