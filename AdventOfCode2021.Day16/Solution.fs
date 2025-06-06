namespace AdventOfCode2021.Day16

open System
open AdventOfCode2021.Common

[<Measure>]
type Index

[<Measure>]
type Count

type Value = UInt64
type Version = int
type TypeId = int

type ParsedPart =
    | LiteralValue of Version * Value
    | Operator of Version * TypeId * ParsedPart list

module Solution =

    let parseInput s =
        s
        |> Seq.map (
            function
            | '0' -> [|
                false
                false
                false
                false
              |]
            | '1' -> [|
                false
                false
                false
                true
              |]
            | '2' -> [|
                false
                false
                true
                false
              |]
            | '3' -> [|
                false
                false
                true
                true
              |]
            | '4' -> [|
                false
                true
                false
                false
              |]
            | '5' -> [|
                false
                true
                false
                true
              |]
            | '6' -> [|
                false
                true
                true
                false
              |]
            | '7' -> [|
                false
                true
                true
                true
              |]
            | '8' -> [|
                true
                false
                false
                false
              |]
            | '9' -> [|
                true
                false
                false
                true
              |]
            | 'A' -> [|
                true
                false
                true
                false
              |]
            | 'B' -> [|
                true
                false
                true
                true
              |]
            | 'C' -> [|
                true
                true
                false
                false
              |]
            | 'D' -> [|
                true
                true
                false
                true
              |]
            | 'E' -> [|
                true
                true
                true
                false
              |]
            | 'F' -> [|
                true
                true
                true
                true
              |]
            | c -> failwith $"Invalid char '{c}'"
        )
        |> Seq.concat
        |> Seq.toArray

    let count (expectedCount: int<Count>) (currentCount: int<Count>) (_: int<Index>) = expectedCount = currentCount

    let length (maxIndex: int<Index>) (_: int<Count>) (currentIndex: int<Index>) = currentIndex >= maxIndex

    let rec getSumOfVersions parsedParts =
        List.fold
            (fun state item ->
                state
                + match item with
                  | LiteralValue(version, _) -> version
                  | Operator(version, _, descendants) -> version + (descendants |> getSumOfVersions)
            )
            0
            parsedParts

    let rec getComputationResult parsedParts =

        let root = parsedParts |> List.exactlyOne

        let rec getNodeValue =
            function
            | LiteralValue(_, value) -> value
            | Operator(_, typeId, descendants) ->

                let for2 (f: 't -> 't -> bool) =
                    List.pairwise >> List.exactlyOne >> Tuple.fold f >> Convert.ToUInt64

                let apply: UInt64 list -> UInt64 =
                    match typeId with
                    | 0 -> List.sum
                    | 1 -> List.product
                    | 2 -> List.min
                    | 3 -> List.max
                    | 5 -> for2 (<)
                    | 6 -> for2 (>)
                    | 7 -> for2 (=)
                    | _ -> failwith $"Invalid TypeId {typeId}"

                descendants |> (List.map getNodeValue) |> apply

        root |> getNodeValue

    let calc calcResult (input: string) =

        let bits = input |> parseInput

        let readInt (startIndex: int<Index>) count =
            let rec read' acc count (index: int<Index>) =
                match count with
                | 0<Count> -> (index, acc)
                | _ ->
                    let v = (acc <<< 1) + (if bits.[index |> int] then 1 else 0)

                    read' v (count - 1<Count>) (index + 1<Index>)

            read' 0 count startIndex

        let readBool startIndex =
            (startIndex + 1<Index>, bits.[startIndex |> int])

        let readLiteralValue (currentIndex: int<Index>) =
            let rec read' currentIndex (acc: UInt64) =
                let currentIndex, flag = readBool currentIndex

                let currentIndex, value = readInt currentIndex 4<Count> |> Tuple.mapSnd uint64

                let value = acc + value

                match flag with
                | false -> (currentIndex, value)
                | true -> read' currentIndex (value <<< 4)

            read' currentIndex 0UL

        let rec exploreData
            (currentIndex: int<Index>)
            (currentCount: int<Count>)
            (returnCondition: int<Count> -> int<Index> -> bool)
            acc
            =
            // read header
            let currentIndex, packetVersion = (readInt currentIndex 3<Count>)
            let currentIndex, packetIdType = readInt currentIndex 3<Count>

            let currentIndex, value =
                match packetIdType with
                | 4 ->
                    readLiteralValue currentIndex
                    |> Tuple.mapSnd (fun v -> LiteralValue(packetVersion, v))
                | _ ->
                    readOperatorBody currentIndex
                    |> Tuple.mapSnd (fun v -> Operator(packetVersion, packetIdType, v))

            let currentCount = currentCount + 1<Count>

            match returnCondition currentCount currentIndex with
            | true -> (currentIndex, (value :: acc))
            | false -> exploreData currentIndex currentCount returnCondition (value :: acc)

        and readOperatorBody currentIndex =
            let currentIndex, flag = readBool currentIndex

            match flag with
            | false ->
                let currentIndex, subPacketsLength = readInt currentIndex 15<Count>

                let stopCondition = (length (currentIndex + (subPacketsLength * 1<Index>)))

                exploreData currentIndex 0<Count> stopCondition []

            | true ->
                let currentIndex, subPacketsCount = readInt currentIndex 11<Count>
                let stopCondition = (count (subPacketsCount * 1<Count>))
                exploreData currentIndex 0<Count> stopCondition []

        let _, parsedParts = exploreData 0<Index> 0<Count> (count 1<Count>) []

        parsedParts |> calcResult

    let part1: string -> int = calc getSumOfVersions
    let part2: string -> UInt64 = calc getComputationResult
