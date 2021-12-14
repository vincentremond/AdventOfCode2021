module AdventOfCode2021.Day12.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day12
open NUnit.Framework
open Swensen.Unquote

let getInputs () = "inputs.txt" |> File.ReadAllLines

let smallSample () =
    (10,
     36,
     "
start-A
start-b
A-c
A-b
b-d
A-end
b-end
")

let largerSample () =
    (19,
     103,
     "
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
")

let eventLargerSample () =
    (226,
     3509,
     "
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
")

[<Test>]
let ``1-1 Test part1 with samples`` () =

    [| smallSample ()
       largerSample ()
       eventLargerSample () |]
    |> Seq.iter (fun (expectedPathsCountPart1, _, inputString) ->
        let asLines =
            inputString
            |> String.splitLines
            |> (Array.filter String.notNullOrEmpty)

        test <@ Solution.part1 asLines = expectedPathsCountPart1 @>)

[<Test>]
let ``1-2 Test part1 with inputs`` () =
    test <@ Solution.part1 (getInputs ()) = 3463 @>

let p2 f =
    let _, expected, data = f ()

    let input =
        data
        |> String.splitLines
        |> (Array.filter String.notNullOrEmpty)

    Assert.AreEqual(expected, (Solution.part2 input))

[<Test>]
let ``2-1-1 Test part1 with samples`` () = p2 smallSample

[<Test>]
let ``2-1-2 Test part1 with samples`` () = p2 largerSample

[<Test>]
let ``2-1-3 Test part1 with samples`` () = p2 eventLargerSample

[<Test>]
let ``2-2 Test part1 with inputs`` () =
    Assert.AreEqual(91533, (Solution.part2 (getInputs ())))

[<EntryPoint>]
let main _ = 0
