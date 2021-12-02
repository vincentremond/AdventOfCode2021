module AdventOfCode2021.Day02.Tests.Part2

open System.IO
open System.Text.RegularExpressions
open AdventOfCode2021.Day02
open NUnit.Framework
open FsUnit

[<SetUp>]
let Setup () = ()

let splitLines s = Regex.Split(s, "[\r\n]+")

[<Test>]
let Test1 () =
    "forward 5
down 5
forward 8
up 3
down 8
forward 2"
    |> splitLines
    |> (Solution.getResult Solution.calculateNewPositionWithAim)
    |> should equal 900

[<Test>]
let Test2 () =
    "inputs.txt"
    |> File.ReadAllLines
    |> (Solution.getResult Solution.calculateNewPositionWithAim)
    |> should equal 1281977850
