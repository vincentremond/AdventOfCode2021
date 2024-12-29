module AdventOfCode2021.Day01.Tests.Part1

open System.IO
open AdventOfCode2021.Day01
open NUnit.Framework
open Swensen.Unquote

[<Test>]
let Test1 () =
    test
        <@
            Solution.calc [|
                199
                200
                208
                210
                200
                207
                240
                269
                260
                263
            |] = 7
        @>

[<Test>]
let Test2 () =
    test <@ "inputs.txt" |> File.ReadAllLines |> Seq.map int |> Solution.calc = 1709 @>
