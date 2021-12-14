module AdventOfCode2021.Day10.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day10
open NUnit.Framework
open Swensen.Unquote

let getSample () =
    "<
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
"
    |> String.splitLines
    |> Array.filter String.notNullOrEmpty

let getInputs () = "inputs.txt" |> File.ReadAllLines

let genericTest sample calc expectedResult = test <@ calc sample = expectedResult @>

[<Test>]
let ``1-1 Test part1 with sample`` () =
    genericTest (getSample ()) Solution.part1 26397

[<Test>]
let ``1-2 Test part1 with inputs`` () =
    genericTest (getInputs ()) Solution.part1 268845

[<Test>]
let ``2-1 Test part1 with sample`` () =
    genericTest (getSample ()) Solution.part2 288957L

[<Test>]
let ``2-2 Test part1 with inputs`` () =
    genericTest (getInputs ()) Solution.part2 4038824534L

[<EntryPoint>]
let main _ = 0
