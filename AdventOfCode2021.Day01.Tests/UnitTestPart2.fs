module AdventOfCode2021.Day01.Tests.Part2

open System.IO
open AdventOfCode2021.Day01
open NUnit.Framework
open Swensen.Unquote

[<Test>]
let Test1 () =
    test
        <@ Solution.calcPart2 [| 199
                                 200
                                 208
                                 210
                                 200
                                 207
                                 240
                                 269
                                 260
                                 263 |] = 5 @>

[<Test>]
let Test2 () =
    test
        <@ "inputs.txt"
           |> File.ReadAllLines
           |> Seq.map int
           |> Solution.calcPart2 = 1761 @>

[<EntryPoint>]
let main _ = 0
