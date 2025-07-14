namespace AdventOfCode2021.Day06

open AdventOfCode2021.Common

module Solution =

    let rec grow days fishes =
        match days with
        | 0 -> fishes
        | remainingDays ->
            grow (remainingDays - 1) [|
                fishes.[1] // 0
                fishes.[2] // 1
                fishes.[3] // 2
                fishes.[4] // 3
                fishes.[5] // 4
                fishes.[6] // 5
                fishes.[7] + fishes.[0] // 6
                fishes.[8] // 7
                fishes.[0]
            |] // 8

    let toArray fishesAge =
        let fishesCountByAge = fishesAge |> Seq.countBy64 id |> Map.ofSeq

        [| for age in 0..1..8 -> fishesCountByAge |> Map.tryFind age |> Option.defaultValue 0L |]

    let calc days values =
        values |> toArray |> grow days |> Array.sum

    let parseInputData lines =
        lines |> Seq.head |> String.split ',' |> Seq.map int

    let getFishCount (days: int) lines = lines |> parseInputData |> (calc days)
