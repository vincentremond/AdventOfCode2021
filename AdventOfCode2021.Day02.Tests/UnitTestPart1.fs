module AdventOfCode2021.Day02.Tests.Part1

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day02
open NUnit.Framework
open Swensen.Unquote

[<Test>]
let Test1 () =
    test
        <@ "forward 5
down 5
forward 8
up 3
down 8
forward 2"
           |> String.splitLines
           |> (Solution.getResult Solution.calculateNewPosition) = 150 @>

[<Test>]
let Test2 () =
    test
        <@ "inputs.txt"
           |> File.ReadAllLines
           |> (Solution.getResult Solution.calculateNewPosition) = 1714950 @>
