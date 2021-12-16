module AdventOfCode2021.Day16.Tests

open System.IO
open AdventOfCode2021.Day16
open NUnit.Framework

let getInputs () = "inputs.txt" |> File.ReadAllText

[<Test>]
let ``1-1-1 Test part1 with sample`` () =
    (16, (Solution.part1 "8A004A801A8002F478"))
    |> Assert.AreEqual

[<Test>]
let ``1-1-2 Test part1 with sample`` () =
    (12, (Solution.part1 "620080001611562C8802118E34"))
    |> Assert.AreEqual

[<Test>]
let ``1-1-3 Test part1 with sample`` () =
    (23, (Solution.part1 "C0015000016115A2E0802F182340"))
    |> Assert.AreEqual

[<Test>]
let ``1-1-4 Test part1 with sample`` () =
    (31, (Solution.part1 "A0016C880162017C3686B18A3D4780"))
    |> Assert.AreEqual

[<Test>]
let ``1-2 Test part1 with inputs`` () =
    (917, (Solution.part1 (getInputs ())))
    |> Assert.AreEqual

[<Test>]
let ``1-2-1 Test part1 with sample`` () =
    (3, (Solution.part2 "C200B40A82"))
    |> Assert.AreEqual

[<Test>]
let ``1-2-2 Test part1 with sample`` () =
    (54, (Solution.part2 "04005AC33890"))
    |> Assert.AreEqual

[<Test>]
let ``1-2-3 Test part1 with sample`` () =
    (7, (Solution.part2 "880086C3E88112"))
    |> Assert.AreEqual

[<Test>]
let ``1-2-4 Test part1 with sample`` () =
    (9, (Solution.part2 "CE00C43D881120"))
    |> Assert.AreEqual

[<Test>]
let ``1-2-5 Test part1 with sample`` () =
    (1, (Solution.part2 "D8005AC2A8F0"))
    |> Assert.AreEqual

[<Test>]
let ``1-2-6 Test part1 with sample`` () =
    (0, (Solution.part2 "F600BC2D8F"))
    |> Assert.AreEqual

[<Test>]
let ``1-2-7 Test part1 with sample`` () =
    (0, (Solution.part2 "9C005AC2F8F0"))
    |> Assert.AreEqual

[<Test>]
let ``1-2-8 Test part1 with sample`` () =
    (1, (Solution.part2 "9C0141080250320F1802104A08"))
    |> Assert.AreEqual

[<Test>]
let ``2-2 Test part1 with inputs`` () =
    (2536453523344UL, (Solution.part2 (getInputs ())))
    |> Assert.AreEqual

[<EntryPoint>]
let main _ = 0
