namespace AdventOfCode2021.Day07

open AdventOfCode2021.Common

module Solution =

    let linear currentPosition targetPosition =
        (currentPosition - targetPosition) |> abs

    let fibonacci currentPosition targetPosition =
        let rec fibonacci' x acc =
            match x with
            | 0 -> acc
            | v -> fibonacci' (v - 1) (acc + v)

        fibonacci' (linear currentPosition targetPosition) 0


    let costAsPos countBy burnCalculator targetPosition =
        countBy
        |> Seq.map (fun (currentPosition, population) ->
            ((burnCalculator currentPosition targetPosition)
             * population)
            |> abs)
        |> Seq.sum

    let calc burnCalculator (resultPicker: int [] -> int) (input: int list) =

        let countBy = input |> Seq.countBy id |> Seq.toArray

        let result =
            [| 0 .. (input |> Seq.max) |]
            |> Array.map (costAsPos countBy burnCalculator)

        printf "%A" result

        result |> resultPicker
