module AdventOfCode2021.Day07.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day07.Solution
open NUnit.Framework
open Swensen.Unquote

let toIntList s =
    s |> String.split ',' |> Seq.map int |> Seq.toList

let getSample () = "16,1,2,0,4,2,7,1,2,14" |> toIntList

let getInputs () =
    "inputs.txt" |> File.ReadAllLines |> Seq.head |> toIntList

let genericTest getSample calc expectedResult =
    test <@ () |> getSample |> calc = expectedResult @>

let cheapest arr = arr |> Array.sort |> Array.head

[<Test>]
let ``1.1 Test part1 with sample`` () =
    genericTest getSample (calc linear cheapest) 37

[<Test>]
let ``1.2 Test part1 with inputs`` () =
    genericTest getInputs (calc linear cheapest) 349769

[<Test>]
let ``2.1 Test part2 with sample`` () =
    genericTest getSample (calc triangular (Array.item 2)) 206

[<Test>]
let ``2.2 Test part2 with inputs`` () =
    genericTest getInputs (calc triangular cheapest) 99540554

[<EntryPoint>]
let main _ = 0
