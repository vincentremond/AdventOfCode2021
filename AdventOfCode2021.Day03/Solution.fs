namespace AdventOfCode2021.Day03

module Solution =
    let getResult (inputs: string []) =
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
