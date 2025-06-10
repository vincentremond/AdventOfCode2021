module AdventOfCode2021.Day15.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day15
open FsUnitTyped
open NUnit.Framework
open Swensen.Unquote

let getSample () =
    "
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
"
    |> String.splitLines
    |> Array.filter String.notNullOrEmpty

let getInputs () = "inputs.txt" |> File.ReadAllLines

[<Test>]
let ``1-1 Test part1 with sample`` () =
    (40, (Solution.part1 (getSample ()))) ||> shouldEqual

[<Test>]
let ``1-2 Test part1 with inputs`` () =
    (755, (Solution.part1 (getInputs ()))) ||> shouldEqual

[<Test>]
let ``2-1 Test part1 with sample`` () =
    (315, (Solution.part2 (getSample ()))) ||> shouldEqual

[<Test>]
let ``2-2 Test part1 with inputs`` () =
    (3016, (Solution.part2 (getInputs ()))) ||> shouldEqual

[<EntryPoint>]
let main _ = 0
