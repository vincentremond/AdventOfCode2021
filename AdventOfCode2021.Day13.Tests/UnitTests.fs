module AdventOfCode2021.Day13.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day13
open FsUnitTyped
open NUnit.Framework
open Swensen.Unquote
open FsUnit

let getSample () =
    "
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"
    |> String.splitLines
    |> Array.filter String.notNullOrEmpty

let getInputs () = "inputs.txt" |> File.ReadAllLines

[<Test>]
let ``1-1 Test part1 with sample`` () =
    (17, (Solution.part1 (getSample ()))) ||> shouldEqual

[<Test>]
let ``1-2 Test part1 with inputs`` () =
    (802, (Solution.part1 (getInputs ()))) ||> shouldEqual

[<Test>]
let ``2-1 Test part1 with sample`` () =
    ("
██████████
██······██
██······██
██······██
██████████",
     (Solution.part2 (getSample ())))
    ||> shouldEqual

[<Test>]
let ``2-2 Test part1 with inputs`` () =
    ("
██████····██····██··██····██··████████··████████····████····██····██··██████··
██····██··██··██····██····██··██··············██··██····██··██····██··██····██
██····██··████······████████··██████········██····██········██····██··██████··
██████····██··██····██····██··██··········██······██··████··██····██··██····██
██··██····██··██····██····██··██········██········██····██··██····██··██····██
██····██··██····██··██····██··██········████████····██████····████····██████··", // RKHFZGUB
     (Solution.part2 (getInputs ())))
    ||> shouldEqual

[<Test>]
let ``Test folding`` () =
    test <@ (Solution.convPoint 7 0) = Some 0 @>
    test <@ (Solution.convPoint 7 14) = Some 0 @>
    test <@ (Solution.convPoint 7 7) = None @>
    test <@ (Solution.convPoint 7 6) = Some 6 @>
    test <@ (Solution.convPoint 7 8) = Some 6 @>

[<EntryPoint>]
let main _ = 0
