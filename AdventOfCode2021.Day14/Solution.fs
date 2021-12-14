namespace AdventOfCode2021.Day14

open AdventOfCode2021.Common

module Solution =

    let parseInputs inputs =
        let rec parseInput' inputs initialConfig mappings =
            match inputs with
            | line :: others ->
                match line with
                | "" -> parseInput' others initialConfig mappings
                | IsMatch @"^(?<Left>\w)(?<Right>\w) -> (?<New>\w)$" m ->
                    let left = m.Groups.["Left"].Value.[0]
                    let right = m.Groups.["Right"].Value.[0]
                    let newValue = m.Groups.["New"].Value.[0]
                    parseInput' others initialConfig (((left, right), newValue) :: mappings)
                | IsMatch @"^(\w+)$" m ->
                    if Option.isSome initialConfig then
                        Error $"Parse error, duplicate initial config '{m.Value}'"
                    else
                        parseInput' others (Some m.Value) mappings
                | _ -> Error $"Parse error, invalid axis '{line}'"
            | [] -> Ok(initialConfig |> Option.get, mappings)

        parseInput' (inputs |> List.ofArray) None []

    let rec doInc (config: ((char * char) * int64) array) mappings incCount =
        match incCount with
        | 0 -> config
        | _ ->
            let newConfig =
                config
                |> Array.map (fun ((l, r), count) ->
                    match (mappings |> Map.tryFind (l, r)) with
                    | None -> failwith "all mapping not exists ??"
                    | Some n -> [| ((l, n), count); ((n, r), count) |])
                |> Array.concat
                |> Array.groupByFstAndMap Array.sum

            doInc newConfig mappings (incCount - 1)


    let calc steps inputs =
        let initialConfig, mappings =
            match parseInputs inputs with
            | Ok resultValue -> resultValue
            | Error error -> failwith error

        let firstLetterToFix = initialConfig |> Seq.head
        let lastLetterToFix = initialConfig |> Seq.last

        let mappings = mappings |> Map.ofSeq

        let currentConfig =
            initialConfig
            |> Seq.pairwise
            |> Seq.countBy64 id
            |> Seq.toArray

        let incremented = doInc currentConfig mappings steps

        let countByChar =
            incremented
            |> Array.map (fun ((l, r), count) -> [| (l, count); (r, count) |])
            |> Array.concat
            |> Array.groupByFstAndMap Array.sum
            |> Array.map (fun (chr, cnt) ->
                let countCorrector =
                    if chr = firstLetterToFix || chr = lastLetterToFix then
                        1L
                    else
                        0L

                (chr, (cnt + countCorrector) / 2L))
            |> Array.sortByDescending snd

        let _, max = Array.head countByChar
        let _, min = Array.last countByChar

        (max - min)


    let part1 = calc 10
    let part2 = calc 40
