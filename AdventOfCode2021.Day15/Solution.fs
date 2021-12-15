namespace AdventOfCode2021.Day15

open AdventOfCode2021.Common

type Position = int * int

type LockedCell = { Y: int; X: int; PathCost: int }

type InProgressCell =
    {
        Y: int
        X: int
        CellCost: int
        PathCost: int
    }

module Solution =

    let asFreeCells (c: seq<seq<(int * int) * int>>) : Map<int * int, int> =
        c
        |> Seq.concat
        |> Seq.filter (fun (pos, _) -> pos <> (0, 0))
        |> Map.ofSeq

    let parseInput inputs =
        inputs
        |> Seq.mapi (fun y l -> l |> Seq.mapi (fun x c -> ((y, x), (c |> ctoi))))
        |> asFreeCells

    let parseInput5x5 inputs =
        let data =
            inputs
            |> Seq.map
                (fun l ->
                    l
                    |> Seq.map (fun c -> ((c |> ctoi) - 1))
                    |> Seq.toArray)
            |> Seq.toArray

        let fiveByFive =
            [| 0 .. 4 |]
            |> Array.map
                (fun yy ->
                    data
                    |> Array.map
                        (fun line ->
                            [| 0 .. 4 |]
                            |> Array.map
                                (fun xx ->
                                    line
                                    |> Array.map (fun v -> ((v + yy + xx) % 9) + 1))
                            |> Array.concat))
            |> Array.concat

        fiveByFive
        |> Seq.mapi (fun y l -> l |> Seq.mapi (fun x v -> ((y, x), v)))
        |> asFreeCells

    let calc (parseInput: string array -> Map<int * int, int>) inputs =

        let freshCells = inputs |> parseInput

        let target =
            freshCells
            |> Map.keys
            |> Seq.fold (Tuple.map2 max) (0, 0)

        let processFresh (locked: LockedCell) (fresh: Map<int * int, int>) =
            let pos = (locked.Y, locked.X)

            let folder (fresh: Map<int * int, int>, inProgress: InProgressCell list) (lookoutPosition: int * int) =

                match fresh |> Map.tryFind lookoutPosition with
                | None -> (fresh, inProgress)
                | Some v ->
                    let newFresh = fresh |> Map.remove lookoutPosition
                    let y, x = lookoutPosition

                    let xx =
                        {
                            Y = y
                            X = x
                            CellCost = v
                            PathCost = locked.PathCost + v
                        }

                    (newFresh, xx :: inProgress)

            [| 0, 1; 0, -1; 1, 0; -1, 0 |]
            |> Array.map (Tuple.map2 (+) pos)
            |> Array.fold folder (fresh, [])

        let processInProgress (locked: LockedCell) inProgress =
            let pos = (locked.Y, locked.X)

            let rec processInProgress' inProgress acc =
                match inProgress with
                | [] -> acc
                | curr :: otherInProgress ->
                    let n =
                        let sum = locked.PathCost + curr.CellCost

                        if sum < curr.PathCost
                           && ((curr.Y + 1, curr.X) = pos
                               || (curr.Y - 1, curr.X) = pos
                               || (curr.Y, curr.X + 1) = pos
                               || (curr.Y, curr.X - 1) = pos) then
                            { curr with PathCost = sum }
                        else
                            curr

                    processInProgress' otherInProgress (n :: acc)

            processInProgress' inProgress []

        let display locked inprogress (fresh: Map<int * int, int>) =
            let height, width = target

            let arr =
                Array.init (height + 1) (fun _ -> Array.init (width + 1) (fun _ -> None))

            let set y x v =
                match arr.[y].[x] with
                | Some _ -> failwith "Duplicate ??"
                | None -> arr.[y].[x] <- Some v

            for x: LockedCell in locked do
                set x.Y x.X $"l\t%d{x.PathCost}"

            for x: InProgressCell in inprogress do
                set x.Y x.X $"i\t%d{x.PathCost}"

            for (y, x), v in fresh |> Map.toSeq do
                set y x $"f\t%d{v}"

            arr
            |> Array.iter
                (fun values ->
                    values
                    |> (Array.map (Option.defaultValue "?\t?"))
                    |> String.join "\t"
                    |> printfn "%s")

        let rec doCell locked (inProgress: InProgressCell list) fresh =
            match inProgress with
            | [] -> failwith "No more in progress ??"
            | next :: otherInProgress ->
                if (next.Y, next.X) = target then
                    display locked inProgress fresh

                    next.PathCost // the final result 🎉
                else
                    let lockedCell =
                        {
                            Y = next.Y
                            X = next.X
                            PathCost = next.PathCost
                        }

                    let newLocked = lockedCell :: locked

                    let stillFresh, nowInProgress = processFresh lockedCell fresh

                    let newInProgress =
                        processInProgress lockedCell otherInProgress

                    // merge and sort
                    let inProgressCells =
                        (nowInProgress @ newInProgress)
                        |> List.sortBy (fun ip -> ip.PathCost)

                    doCell newLocked inProgressCells stillFresh

        doCell
            []
            [
                {
                    Y = 0
                    X = 0
                    CellCost = 0
                    PathCost = 0
                }
            ]
            freshCells

    let part1: string array -> int = calc parseInput
    let part2: string array -> int = calc parseInput5x5
