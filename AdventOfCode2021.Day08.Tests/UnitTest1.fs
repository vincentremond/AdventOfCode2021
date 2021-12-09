module AdventOfCode2021.Day06.Tests

open System.IO
open AdventOfCode2021.Common
open AdventOfCode2021.Day08
open NUnit.Framework
open Swensen.Unquote

let getSample () =
    "
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
"
    |> String.splitLines
    |> Seq.filter String.notNullOrEmpty

let getInputs () = "inputs.txt" |> File.ReadAllLines

let genericTest sample calc expectedResult = test <@ calc sample = expectedResult @>

let cheapest arr = arr |> Array.sort |> Array.head

[<Test>]
let ``1.1 Test part1 with sample`` () =
    genericTest (getSample ()) Solution.part1 26

[<Test>]
let ``1.2 Test part1 with inputs`` () =
    genericTest (getInputs ()) Solution.part1 445

[<Test>]
let Test21 () =
    let data = getSample ()
    let part2 = Solution.part2 data
    Assert.AreEqual(61229, part2)

[<Test>]
let Test22 () =
    let data = getInputs ()
    let part2 = Solution.part2 data
    Assert.AreEqual(61229, part2)
