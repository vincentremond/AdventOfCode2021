module AdventOfCode2021.Day06.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day06
open NUnit.Framework
open FsUnit


let getSample () =
    "3,4,3,1,2" |> String.splitLines |> Array.toList

let getInputs () =
    "inputs.txt" |> File.ReadAllLines |> Array.toList

let genericTest getSample calc days expectedResult =
    ()
    |> getSample
    |> calc days
    |> should equal expectedResult

[<Test>]
let ``1.1 Test part1 with sample`` () =
    genericTest getSample Solution.getFishCount 80 5934L

[<Test>]
let ``1.2 Test part1 with inputs`` () =
    genericTest getInputs Solution.getFishCount 80 388739L

[<Test>]
let ``2.1 Test part2 with sample`` () =
    genericTest getSample Solution.getFishCount 256 26984457539L

[<Test>]
let ``2.2 Test part2 with inputs`` () =
    genericTest getInputs Solution.getFishCount 256 1741362314973L
