namespace AdventOfCode2021.Day05

open AdventOfCode2021.Common
open Microsoft.FSharp.Core
open FSharp.Text.RegexProvider

module Solution =

    type Line =
        { Start: (int * int)
          End: (int * int) }

    type LineParser = Regex< @"^(?<X1>\d+),(?<Y1>\d+) -> (?<X2>\d+),(?<Y2>\d+)$" >

    let horizontalOrVertical line =
        match Tuple.map2 (-) line.Start line.End with
        | _, 0 -> true
        | 0, _ -> true
        | _ -> false

    let horizontalOrVerticalOrDiagonal line =
        match Tuple.map2 (-) line.Start line.End |> Tuple.map abs with
        | _, 0 -> true
        | 0, _ -> true
        | diffX, diffY -> diffX = diffY

    let calc lineFilter (input: string seq) =

        let getInc s e =
            let cmp x =
                match x with
                | x' when x' > 0 -> (-1, (<))
                | x' when x' < 0 -> (1, (>))
                | 0 -> (0, (<>))
                | _ -> failwith "todo"

            Tuple.map2 (-) s e |> Tuple.map cmp

        let forTuple s endOfLine =
            let increments, operators = (getInc s endOfLine) |> Tuple.transpose

            let generator currentPoint =
                match Tuple.apply2 operators currentPoint endOfLine with
                | false, false ->
                    let nextPoint = Tuple.map2 (+) currentPoint increments
                    Some(currentPoint, nextPoint)
                | _ -> None

            Seq.unfold generator s

        let expandLine line = forTuple (line.Start) (line.End)

        let lines =
            input
            |> Seq.map (LineParser().TypedMatch)
            |> Seq.map
                (fun regexMatch ->
                    { Start =
                          (regexMatch.X1.Value, regexMatch.Y1.Value)
                          |> Tuple.map int
                      End =
                          (regexMatch.X2.Value, regexMatch.Y2.Value)
                          |> Tuple.map int })
            |> Seq.filter lineFilter
            |> Seq.toList

        let allPoints =
            lines
            |> Seq.map expandLine
            |> Seq.concat
            |> Seq.toList

        let countBy = allPoints |> Seq.countBy id

        countBy
        |> Seq.filter (fun (_, count) -> count > 1)
        |> Seq.map snd
        |> Seq.length

    let part1: seq<string> -> int = (calc horizontalOrVertical)
    let part2: seq<string> -> int = (calc horizontalOrVerticalOrDiagonal)
