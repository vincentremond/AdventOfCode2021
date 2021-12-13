namespace AdventOfCode2021.Day13

open System.Text
open AdventOfCode2021.Common

type X = int
type Y = int

type Point = Y * X

type FoldingLine =
    | Horizontal of Y
    | Vertical of X

module Solution =
    let parseInput lines =
        let rec parseInput' lines points folds =
            match lines with
            | line :: others ->
                match line with
                | "" -> parseInput' others points folds
                | IsMatch @"^(?<X>\d+),(?<Y>\d+)$" m ->
                    let point: Point =
                        (m.Groups.["Y"].Value |> int, m.Groups.["X"].Value |> int)

                    parseInput' others (point :: points) folds
                | IsMatch @"^fold along (?<Axis>\w)=(?<Distance>\d+)$" m ->
                    let distance = m.Groups.["Distance"].Value |> int

                    let fold =
                        let axis = m.Groups.["Axis"].Value

                        match axis with
                        | "x" -> Vertical distance
                        | "y" -> Horizontal distance
                        | _ -> failwith $"Parse error, invalid axis '{axis}'"

                    parseInput' others points (fold :: folds)
                | _ -> failwith $"Parse error, invalid axis '{line}'"
            | [] -> (points, folds |> List.rev)

        parseInput' lines [] []

    let convPoint foldA a =
        match (a - foldA) with
        | 0 -> None
        | a' ->
            let i = foldA - (a' |> abs)

            if i >= 0 then
                Some(i)
            else
                failwith "Humm 🤔"

    let mapTuple (f1, f2) (a1, a2) = (f1 a1, f2 a2)

    let foldPoint fold =
        (match fold with
         | Vertical x -> mapTuple (Some, (convPoint x))
         | Horizontal y -> mapTuple ((convPoint y), Some))
        >> Option.unfold

    let displayChars (points: Point list) : string =
        let pointsSet: Point Set = Set.ofList points

        let getChars () =
            seq {
                let maxY, maxX =
                    List.fold (fun state item -> Tuple.map2 max item state) (0, 0) points

                for y in 0 .. maxY do
                    yield "\r\n"
                    for x in 0 .. maxX do
                        yield (
                         match pointsSet |> Set.contains (y, x) with
                         | true -> "██"
                         | false -> "··")
            }

        let result = getChars() |> String.concat String.empty
        result



    let calc (foldFilter: FoldingLine list -> FoldingLine list) (calculateResult: Point list -> 'a) inputs =

        let points, folds = inputs |> Seq.toList |> parseInput
        let folds = folds |> foldFilter

        let folder (points: Point list) (paperFold: FoldingLine) : Point list =
            points
            |> Seq.choose (foldPoint paperFold)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toList

        let finalPoints = List.fold folder points folds

        finalPoints |> calculateResult

    let part1: string seq -> int = calc (List.take 1) (List.length)
    let part2: string seq -> string = calc id displayChars
