namespace AdventOfCode2021.Day10

open AdventOfCode2021.Common

module Solution =

    let part1 (lines: string array) =

        let starters =
            [ '(', ')'
              '[', ']'
              '{', '}'
              '<', '>' ]
            |> Map.ofList

        let scores =
            [ ')', 3
              ']', 57
              '}', 1197
              '>', 25137 ]
            |> Map.ofList

        let rec findFirstIllegalChar starts chars =
            match chars with
            | c :: otherChars ->
                match Map.tryFind c starters with
                | Some _ -> findFirstIllegalChar (c :: starts) otherChars
                | None ->
                    match starts with
                    | start :: otherStarts ->
                        let ending = Map.find start starters

                        if ending = c then
                            findFirstIllegalChar otherStarts otherChars
                        else
                            Some c
                    | [] -> None
            | [] -> None

        lines
        |> Array.choose (List.ofSeq >> (findFirstIllegalChar []))
        |> Array.map (Map.findI scores)
        |> Array.sum

    let part2 (lines: string array) =

        let starters =
            [ '(', ')'
              '[', ']'
              '{', '}'
              '<', '>' ]
            |> Map.ofList

        let scores =
            [ ')', 3
              ']', 57
              '}', 1197
              '>', 25137 ]
            |> Map.ofList

        let calculateScoreForMissing stack =
            let rec calculateScoreForMissing' stack score =
                match stack with
                | c :: others ->
                    let newScore =
                        score * 5L
                        + match c with
                          | '(' -> 1L
                          | '[' -> 2L
                          | '{' -> 3L
                          | '<' -> 4L
                          | _ -> failwith $"Invalid char %A{c}"

                    calculateScoreForMissing' others newScore
                | [] -> Some score

            calculateScoreForMissing' stack 0L

        let calculateScoreForInvalid _ = None

        let rec findFirstIllegalChar starts chars =
            match chars with
            | c :: otherChars ->
                match Map.tryFind c starters with
                | Some _ -> findFirstIllegalChar (c :: starts) otherChars
                | None ->
                    match starts with
                    | start :: otherStarts ->
                        let ending = Map.find start starters

                        if ending = c then
                            findFirstIllegalChar otherStarts otherChars
                        else
                            calculateScoreForInvalid c
                    | [] -> failwith "Err"
            | [] -> calculateScoreForMissing starts

        let getMedian values =
            let index = Array.length values / 2
            let sorted = values |> Array.sort
            sorted.[index]

        lines
        |> Array.choose (List.ofSeq >> (findFirstIllegalChar []))
        |> getMedian
