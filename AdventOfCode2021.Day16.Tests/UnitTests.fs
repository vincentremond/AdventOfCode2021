module AdventOfCode2021.Day16.Tests

open System.IO
open AdventOfCode2021.Day16
open FsUnitTyped
open NUnit.Framework

let getInputs () = "inputs.txt" |> File.ReadAllText

[<Test>]
let ``1-1-1 Test part1 with sample`` () =
    (16, (Solution.part1 "8A004A801A8002F478")) ||> shouldEqual

[<Test>]
let ``1-1-2 Test part1 with sample`` () =
    (12, (Solution.part1 "620080001611562C8802118E34")) ||> shouldEqual

[<Test>]
let ``1-1-3 Test part1 with sample`` () =
    (23, (Solution.part1 "C0015000016115A2E0802F182340")) ||> shouldEqual

[<Test>]
let ``1-1-4 Test part1 with sample`` () =
    (31, (Solution.part1 "A0016C880162017C3686B18A3D4780")) ||> shouldEqual

[<Test>]
let ``1-2 Test part1 with inputs`` () =
    (917, (Solution.part1 (getInputs ()))) ||> shouldEqual

[<Test>]
let ``1-2-1 Test part1 with sample`` () =
    (3UL, (Solution.part2 "C200B40A82")) ||> shouldEqual

[<Test>]
let ``1-2-2 Test part1 with sample`` () =
    (54UL, (Solution.part2 "04005AC33890")) ||> shouldEqual

[<Test>]
let ``1-2-3 Test part1 with sample`` () =
    (7UL, (Solution.part2 "880086C3E88112")) ||> shouldEqual

[<Test>]
let ``1-2-4 Test part1 with sample`` () =
    (9UL, (Solution.part2 "CE00C43D881120")) ||> shouldEqual

[<Test>]
let ``1-2-5 Test part1 with sample`` () =
    (1UL, (Solution.part2 "D8005AC2A8F0")) ||> shouldEqual

[<Test>]
let ``1-2-6 Test part1 with sample`` () =
    (0UL, (Solution.part2 "F600BC2D8F")) ||> shouldEqual

[<Test>]
let ``1-2-7 Test part1 with sample`` () =
    (0UL, (Solution.part2 "9C005AC2F8F0")) ||> shouldEqual

[<Test>]
let ``1-2-8 Test part1 with sample`` () =
    (1UL, (Solution.part2 "9C0141080250320F1802104A08")) ||> shouldEqual

[<Test>]
let ``2-2 Test part1 with inputs`` () =
    (2536453523344UL, (Solution.part2 (getInputs ()))) ||> shouldEqual

[<EntryPoint>]
let main _ = 0
