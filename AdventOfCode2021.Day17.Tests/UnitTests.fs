module AdventOfCode2021.Day17.Tests

open AdventOfCode2021.Day17
open NUnit.Framework

let getSample () ="target area: x=20..30, y=-10..-5"

let getInputs () = "target area: x=217..240, y=-126..-69"

[<Test>]
let ``1-1 Test part1 with sample`` () = (45 , (Solution.part1 (getSample ()))) |> Assert.AreEqual
[<Test>]
let ``1-2 Test part1 with inputs`` () = (7875 , (Solution.part1 (getInputs ()))) |> Assert.AreEqual

[<Test>]
let ``2-1 Test part1 with sample`` () = (112 , (Solution.part2 (getSample ()))) |> Assert.AreEqual
[<Test>]
let ``2-2 Test part1 with inputs`` () = (2321 , (Solution.part2 (getInputs ()))) |> Assert.AreEqual

[<EntryPoint>]
let main _ = 0
