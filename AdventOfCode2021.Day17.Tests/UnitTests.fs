module AdventOfCode2021.Day17.Tests

open AdventOfCode2021.Day17
open FsUnitTyped
open NUnit.Framework
open FsUnit

let getSample () = "target area: x=20..30, y=-10..-5"

let getInputs () = "target area: x=217..240, y=-126..-69"

[<Test>]
let ``1-1 Test part1 with sample`` () =
    (45<VPos>, (Solution.part1 (getSample ()))) ||> shouldEqual

[<Test>]
let ``1-2 Test part1 with inputs`` () =
    (7875<VPos>, (Solution.part1 (getInputs ()))) ||> shouldEqual

[<Test>]
let ``2-1 Test part1 with sample`` () =
    (112, (Solution.part2 (getSample ()))) ||> shouldEqual

[<Test>]
let ``2-2 Test part1 with inputs`` () =
    (2321, (Solution.part2 (getInputs ()))) ||> shouldEqual

[<EntryPoint>]
let main _ = 0
