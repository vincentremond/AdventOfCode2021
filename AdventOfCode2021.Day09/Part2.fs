module AdventOfCode2021.Day09.Part2

open AdventOfCode2021.Common

type BasinId = int * int

type BasinResult =
    | Unassigned
    | Peak
    | Basin of BasinId

let part2 (lines: string array) =
    let map =
        function
        | '9' -> Peak
        | _ -> Unassigned

    let data = lines |> Array.map (Seq.map map >> Seq.toArray)

    let directions = [|
        0, 1
        1, 0
        -1, 0
        0, -1
    |]

    let validateIndex max index = index < max && index >= 0

    let rec exploreBasin y x row id =
        data.[y].[x] <- Basin id

        let max = (Array.length data, Array.length row)

        let position = (y, x)

        directions
        |> Seq.map (Tuple.map2 (+) position)
        |> Seq.filter (Tuple.map2 validateIndex max >> Tuple.``and``)
        |> Seq.filter (fun (y, x) ->
            match data.[y].[x] with
            | Unassigned -> true
            | _ -> false
        )
        |> Seq.iter (fun (y, x) -> exploreBasin y x row id)

    data
    |> Array.iteri (fun y row ->
        row
        |> Array.iteri (fun x value ->
            match value with
            | Peak
            | Basin _ -> ()
            | Unassigned -> exploreBasin y x row (y, x)
        )
    )

    data
    |> Array.concat
    |> Array.choose (
        (fun b ->
            match b with
            | Basin id -> Some id
            | _ -> None
        )
    )
    |> Array.countBy id
    |> Array.sortByDescending snd
    |> Array.map snd
    |> Array.take 3
    |> Array.fold (fun state item -> item * state) 1
