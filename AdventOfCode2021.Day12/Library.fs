namespace AdventOfCode2021.Day12

open System
open FSharp.Text.RegexProvider
open AdventOfCode2021.Common

type LineParser = Regex<"^(?<From>\w+)-(?<To>\w+)$">

type CaveId = string

type Cave =
    | Start
    | End
    | LargeCave of CaveId
    | SmallCave of CaveId

module Solution =

    let (|AllUpperCase|AllLowerCase|) (s: string) =
        if s |> Seq.forall Char.IsUpper then
            AllUpperCase
        else
            AllLowerCase

    let calc lines canUseJoker =

        let toCave s =
            match s with
            | "start" -> Start
            | "end" -> End
            | AllUpperCase -> LargeCave s
            | AllLowerCase -> SmallCave s

        let readLine l =
            let r = l |> (LineParser().TypedMatch)

            [|
                (toCave r.From.Value, toCave r.To.Value)
                (toCave r.To.Value, toCave r.From.Value)
            |]

        let caves =
            lines
            |> Array.collect readLine
            |> Array.groupByFst
            |> Map.ofArray

        let displayPath path =
            path
            |> Seq.rev
            |> Seq.iter
                (fun p ->
                    let itemValue =
                        match p with
                        | Start -> "start"
                        | End -> "end"
                        | SmallCave s -> s
                        | LargeCave l -> l

                    printf "%s," itemValue)

            printfn "End"

        let rec explore cave exploredSmallCaves jokerUsed path =
            let newPath = cave :: path

            let exploredSmallCaves =
                match cave with
                | SmallCave smallCaveId ->
                    match (Set.contains smallCaveId exploredSmallCaves) with
                    | false -> Set.add smallCaveId exploredSmallCaves
                    | _ -> exploredSmallCaves
                | _ -> exploredSmallCaves

            let possibleDirections = caves |> Map.find cave

            possibleDirections
            |> Array.sumBy
                (fun targetCave ->
                    match targetCave with
                    | Start -> 0
                    | End ->
                        displayPath newPath
                        1
                    | LargeCave _ -> explore targetCave exploredSmallCaves jokerUsed newPath
                    | SmallCave smallCaveId ->
                        let explored =
                            Set.contains smallCaveId exploredSmallCaves

                        match explored, jokerUsed, canUseJoker with
                        | true, false, true -> explore targetCave exploredSmallCaves true newPath
                        | true, true, true -> 0
                        | true, _, false -> 0
                        | false, _, _ -> explore targetCave exploredSmallCaves jokerUsed newPath
                )

        explore Start Set.empty false []

    let part1 lines = calc lines false
    let part2 lines = calc lines true
