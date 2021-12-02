namespace AdventOfCode2021.Day01

module Solution =
    let calc inputs =
        inputs
        |> Seq.pairwise
        |> Seq.filter (fun (a, b) -> (b > a))
        |> Seq.length

    let calcPart2 (inputs: int seq) =
        inputs
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> calc
