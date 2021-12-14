module AdventOfCode2021.Day14.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day14
open NUnit.Framework

let getSample () =
    "
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
"
    |> String.splitLines
    |> Array.filter String.notNullOrEmpty

let getInputs () = "inputs.txt" |> File.ReadAllLines

[<Test>]
let ``1-1 Test part1 with sample`` () = (1588 , (Solution.part1 (getSample ()))) |> Assert.AreEqual
[<Test>]
let ``1-2 Test part1 with inputs`` () = (2915 , (Solution.part1 (getInputs ()))) |> Assert.AreEqual

[<Test>]
let ``2-1 Test part1 with sample`` () = (2188189693529L , (Solution.part2 (getSample ()))) |> Assert.AreEqual
[<Test>]
let ``2-2 Test part1 with inputs`` () = (3353146900153L , (Solution.part2 (getInputs ()))) |> Assert.AreEqual

[<EntryPoint>]
let main _ = 0
