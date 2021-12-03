namespace AdventOfCode2021.Day03

open AdventOfCode2021.Common

module Solution =
    let getResultPart1 (inputs: string seq) =
        let data =
            inputs
            |> Seq.map
                (fun line ->
                    line
                    |> Seq.map
                        (fun c ->
                            match c with
                            | '0' -> -1
                            | '1' -> 1
                            | c -> failwith $"Invalid char {c}"))
            |> Seq.transpose
            |> Seq.map Seq.sum
            |> Seq.rev
            |> Seq.toArray

        let toInt comp arr : int =
            arr
            |> Seq.mapi (fun index value -> if comp 0 value then 0 else 1 <<< index)
            |> Seq.sum

        let gamma = data |> toInt (>)
        let epsilon = data |> toInt (<)
        let power = gamma * epsilon
        power

    let getResultPart2 (inputs: string seq) =



        let data =
            inputs
            |> Seq.map
                (fun line ->
                    line
                    |> Seq.map
                        (fun c ->
                            match c with
                            | '0' -> -1
                            | '1' -> 1
                            | c -> failwith $"Invalid char {c}")
                    |> Seq.toList)
            |> Seq.toList

        let rec calc result (d: int list list) operator =
            if List.length d = 0 then
                failwith "error"

            let data =
                d |> (List.map List.tryPop) |> Option.ofList

            let fix v =
                match v with
                | -1 -> 0
                | 1 -> 1
                | _ -> failwith "todo"

            match data with
            | None -> result
            | Some values ->
                let valueToAdd, filter =
                    if List.length values = 1 then
                        (values |> List.map fst |> List.exactlyOne |> fix, (fun (v, _) -> true))
                    else
                        let sum = values |> List.map fst |> List.sum

                        if operator sum 0 then
                            (1, (fun (v, _) -> v = 1))
                        else
                            (0, (fun (v, _) -> v = -1))

                let nextValues =
                    (values |> List.filter filter |> List.map snd)

                calc (valueToAdd :: result) nextValues operator

        let toInt (lst: int list) =
            lst
            |> Seq.mapi (fun index item -> item <<< index)
            |> Seq.sum

        let oxygen = calc [] data (>=) |> toInt
        let co2 = calc [] data (<) |> toInt

        oxygen * co2
