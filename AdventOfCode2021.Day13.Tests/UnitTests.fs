module AdventOfCode2021.Day13.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day13
open NUnit.Framework
open Swensen.Unquote

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
let ``1-1 Test part1 with sample`` () = (-1 , (Solution.part1 (getSample ()))) |> Assert.AreEqual
[<Test>]
let ``1-1 Test part1 with inputs`` () = (-1 , (Solution.part1 (getInputs ()))) |> Assert.AreEqual

[<Test>]
let ``2-1 Test part1 with sample`` () = (-1 , (Solution.part2 (getSample ()))) |> Assert.AreEqual
[<Test>]
let ``2-2 Test part1 with inputs`` () = (-1 , (Solution.part2 (getInputs ()))) |> Assert.AreEqual
