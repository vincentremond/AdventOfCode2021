namespace AdventOfCode2021.Day04

open AdventOfCode2021.Common

[<Struct>]
type DrawnNumber = {
    Position: int
    Value: int
}

type DrawnNumbers = DrawnNumber list

type RowToWin = {
    MaxDrownNumber: DrawnNumber
    Numbers: DrawnNumber list
}

type Grid = {
    DrawnNumbers: DrawnNumbers
    LowestDrownNumber: DrawnNumber
    RowsToWins: RowToWin list
}

type Grids = Grid list

module Solution =

    let gridSize = 5

    let winningPositions =
        let rows =
            seq { for x in 0 .. (gridSize - 1) -> [| for y in 0 .. (gridSize - 1) -> (x, y) |] }

        let columns =
            seq { for y in 0 .. (gridSize - 1) -> [| for x in 0 .. (gridSize - 1) -> (x, y) |] }

        let diag1 = [| for x in 0 .. (gridSize - 1) -> (x, x) |]

        let diag2 = [| for x in (gridSize - 1) .. -1 .. 0 -> (x, x) |]

        seq {
            yield! rows
            yield! columns
            yield diag1
            yield diag2
        }
        |> Seq.toArray

    let mapGrids (drawnNumbers: Map<int, DrawnNumber>) (rawGrids: int list list list) : Grids =

        let mapNumbers rawGrid (numbers: (int * int)[]) : RowToWin =
            let numbers =
                numbers
                |> Seq.map (fun (x, y) ->
                    let value = rawGrid |> List.item x |> List.item y
                    drawnNumbers |> Map.find value
                )
                |> Seq.toList

            let max = numbers |> Seq.sortByDescending (fun x -> x.Position) |> Seq.head

            {
                MaxDrownNumber = max
                Numbers = numbers
            }

        let mapRows (rawGrid: int list list) : RowToWin list =
            winningPositions |> Seq.map (mapNumbers rawGrid) |> Seq.toList

        let mapGrid (rawGrid: int list list) : Grid =

            let drawnNumbers =
                rawGrid |> List.concat |> List.map (fun x -> drawnNumbers |> Map.find x)

            let rowsToWins = rawGrid |> mapRows

            let maxDrownNumber =
                rowsToWins |> Seq.sortBy (fun r -> r.MaxDrownNumber.Position) |> Seq.head

            {
                DrawnNumbers = drawnNumbers
                LowestDrownNumber = maxDrownNumber.MaxDrownNumber
                RowsToWins = rowsToWins
            }

        rawGrids |> List.map mapGrid

    let transformInput (lines: string list) : Grids =

        let drawnNumbersAsText, allGrids = lines |> List.pop

        let drawnNumbers =
            drawnNumbersAsText
            |> String.split ','
            |> Array.map int
            |> Array.mapi (fun index drawnNumber ->
                (drawnNumber,
                 {
                     Position = index
                     Value = drawnNumber
                 })
            )
            |> Map.ofSeq

        allGrids
        |> List.chunkBySize 6
        |> List.map (List.skip 1)
        |> List.map (fun grid ->
            grid
            |> List.map (fun line ->
                line
                |> Seq.chunkBySize 3
                |> Seq.map (fun chars -> new string (chars) |> int)
                |> Seq.toList
            )
        )
        |> (mapGrids drawnNumbers)

    type UpdateGridResult =
        | Winner of Grid
        | NoWinner of Grids

    let getResult seqSort (lines: string list) =
        let grids = transformInput lines

        let winningGrid =
            grids |> seqSort (fun g -> g.LowestDrownNumber.Position) |> Seq.head

        let winningNumber = winningGrid.LowestDrownNumber

        let split =
            winningGrid.DrawnNumbers
            |> List.partition (fun drawnNumber -> drawnNumber.Position <= winningNumber.Position)

        let _, b = split |> Tuple.map (List.sumBy (fun x -> x.Value))

        winningNumber.Value * b

    let getResultPart1 = (getResult Seq.sortBy)
    let getResultPart2 = (getResult Seq.sortByDescending)
