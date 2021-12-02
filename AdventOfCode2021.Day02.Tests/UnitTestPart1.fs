module AdventOfCode2021.Day02.Tests.Part1

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
    |> (Solution.getResult Solution.calculateNewPosition)
    |> should equal 150

[<Test>]
let Test2 () =
    "inputs.txt"
    |> File.ReadAllLines
    |> (Solution.getResult Solution.calculateNewPosition)
    |> should equal 1714950
