namespace AdventOfCode2021.Day11

open System
open AdventOfCode2021.Common

module Solution =
    let calc f lines =

        let data =
            lines
            |> Seq.map (Seq.map ctoi >> Seq.toArray)
            |> Seq.toArray

        let mod10 i = i % 10

        let displayData data =
            data
            |> Seq.map ((Array.map (mod10 >> itoc)) >> String)
            |> String.join Environment.NewLine

        let max =
            (Array.length data, Array.length data.[0])

        let neighbours =
            [|
                0, 1
                1, 1
                1, 0
                1, -1
                0, -1
                -1, -1
                -1, 0
                -1, 1
            |]

        let getNeighboursToIncrement position =
            let add max v inc =
                let n = v + inc

                if n < 0 then None
                else if n >= max then None
                else Some n

            neighbours
            |> Array.choose (Tuple.map3 add max position >> Option.unfold)

        let rec incrementOctopus iteration (y, x) =
            let v = data.[y].[x]
            let i = iteration * 10

            let newValue, shouldIncrementNeighbours =
                match v % 10 with
                | 0 when v = i -> v, false
                | 0 -> i + 1, false
                | 9 -> i + 0, true
                | v -> v + 1, false

            data.[y].[x] <- newValue

            if shouldIncrementNeighbours then
                getNeighboursToIncrement (y, x)
                |> Seq.iter (incrementOctopus iteration)

        let incrementOctopuses iteration =
            data
            |> Array.iteri
                (fun y row ->
                    row
                    |> Array.iteri (fun x _ -> incrementOctopus iteration (y, x)))

            if iteration < 10 || iteration % 10 = 0 then
                Console.WriteLine($"After step {iteration}:")
                data |> displayData |> Console.WriteLine
                Console.WriteLine()

            data
            |> Seq.concat
            |> Seq.map mod10
            |> Seq.count (fun v -> v = 0)

        Console.WriteLine("Before any steps:")
        data |> displayData |> Console.WriteLine
        Console.WriteLine()

        f incrementOctopuses


    let part1 lines =

        let getResult incOctopuses =
            [| 1 .. 100 |]
            |> Array.map incOctopuses
            |> Array.sum

        calc getResult lines

    let part2 lines =

        let targetLen = lines |> Seq.concat |> Seq.length

        let getResult incOctopuses =
            let rec xxx i =
                if (incOctopuses i) = targetLen then
                    i
                else
                    xxx (i + 1)

            xxx 1

        calc getResult lines
