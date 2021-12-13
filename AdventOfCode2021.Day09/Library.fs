namespace AdventOfCode2021.Day09

open System
open Microsoft.FSharp.Core
open AdventOfCode2021.Common

module Solution =

    type RowIndex = int
    type StartIndex = int
    type Length = int

    type Basin = RowIndex * StartIndex * Length

    type ChildBasinAffectResult =
        | AlreadyRecognisedByAnother of int
        | ChildRecognised of Map<Basin, int>

    let parse (lines: string array) =
        let ctoi (c: char) =
            Convert.ToInt32(c) - Convert.ToInt32('0')

        lines |> Array.map (Seq.map ctoi >> Seq.toArray)

    let part1 lines : int =
        let data = lines |> parse

        let neighborsLocation = [| -1, 0; 1, 0; 0, -1; 0, 1 |]

        let validateIndex length index : int option =
            if index < 0 then None
            else if index >= length then None
            else Some index


        data
        |> Seq.mapi (fun y row ->
            let maxIndices = (Array.length data, Array.length row)

            row
            |> Seq.mapi (fun x value ->
                let currentPosition = (y, x)

                let isDeeperThanNeighbours =
                    neighborsLocation
                    |> Seq.map (fun neighborDirection ->
                        neighborDirection
                        |> Tuple.map2 (+) currentPosition
                        |> Tuple.map2 validateIndex maxIndices
                        |> Option.unfold)
                    |> Seq.choose id
                    |> Seq.forall (fun (y, x) -> data.[y].[x] > value)

                if isDeeperThanNeighbours then
                    Some(currentPosition, (value + 1))
                else
                    None))
        |> Seq.concat
        |> Seq.choose id
        |> Seq.map snd
        |> Seq.toList
        |> Seq.sum

    let part2 lines =
        let parse (lines: string array) =
            let ctoi (c: char) =
                (Convert.ToInt32(c) - Convert.ToInt32('0'))

            lines |> Array.map (Seq.map ctoi >> Seq.toList)

        let data = lines |> parse

        let perRow rowIndex row =
            let rec perRow' row currentPartialBasin position acc =
                match row with
                | currentItem :: others ->
                    match currentItem, currentPartialBasin with
                    | 9, Some currentBasin ->
                        // terminate a basin
                        perRow' others None (position + 1) (currentBasin :: acc)
                    | 9, None ->
                        // continue without basin, nothing to see
                        perRow' others None (position + 1) acc
                    | v, Some (rowIndex, startIndex, length) ->
                        // extend a basin
                        perRow' others (Some(rowIndex, startIndex, length + 1)) (position + 1) acc
                    | v, None ->
                        // start a new basin
                        perRow' others (Some(rowIndex, position, 1)) (position + 1) acc
                | [] ->
                    match currentPartialBasin with
                    | Some partialBasin -> partialBasin :: acc
                    | None -> acc

            perRow' row None 0 [] |> List.rev


        let associateNeighbourBasins (row1: Basin list, row2: Basin list) =
            List.allPairs row1 row2
            |> List.filter (fun ((r1, s1, l1), (r2, s2, l2)) -> Tool.intersect (s1, (s1 + l1 - 1)) (s2, (s2 + l2 - 1)))

        //            for basin1 in row1 do
//                let (r1, s1, l1) = basin1
//                let e1 = s1 + l1 - 1
//
//                let intersect =
//                    row2
//                    |> List.filter
//                        (fun (r2, s2, l2) ->
//                            let e2 = s2 + l2 - 1
//                            Tool.intersect s1 e1 s2 e2)
//                    |> List.map (Tuple.mk basin1)
//
//                failwith "TODO"


        let basinsPerRow: Basin list array = data |> Array.mapi perRow

        let linkedRowBasins =
            basinsPerRow
            |> Array.pairwise
            |> Array.map associateNeighbourBasins
            |> Seq.concat
            |> Seq.groupBy fst
            |> Seq.map (Tuple.mapSnd (Seq.map snd >> Seq.toList))
            |> Seq.toArray
            |> Map.ofSeq

        let rec tryAssignConsecutiveBasins basin largeBasinId map : ChildBasinAffectResult =
            match Map.tryFind basin linkedRowBasins with
            | Some linkedBasins ->
                match map |> Map.tryFind basin with
                | Some largeBasinId -> AlreadyRecognisedByAnother largeBasinId
                | None ->
                    let folder (map) (basin: Basin) =
                        match map with
                        | AlreadyRecognisedByAnother i -> AlreadyRecognisedByAnother i
                        | ChildRecognised map ->
                            match tryAssignConsecutiveBasins basin largeBasinId map with
                            | AlreadyRecognisedByAnother i -> AlreadyRecognisedByAnother i
                            | ChildRecognised map -> ChildRecognised(Map.add basin largeBasinId map)

                    linkedBasins
                    |> List.fold folder (ChildRecognised map)
            | None -> ChildRecognised map

        let rec reExplore basins map largeBasinId =
            match basins with
            | basin :: others ->
                match map |> Map.tryFind basin with
                | Some alreadyAffectedId -> reExplore others map (largeBasinId + 1)
                | None ->
                    let newMap =
                        match tryAssignConsecutiveBasins basin largeBasinId map with
                        | AlreadyRecognisedByAnother i -> Map.add basin i map
                        | ChildRecognised map -> Map.add basin largeBasinId map
                    //                    let mapWithAffectedChildren = tryAssignConsecutiveBasins basin largeBasinId newMap
                    reExplore others newMap (largeBasinId + 1)
            | [] -> map

        let product (values: int list) =
            let rec product' acc values =
                match values with
                | v :: others -> product' (acc * v) others
                | [] -> acc

            product' 1 values

        let int32s =
            reExplore (basinsPerRow |> Seq.concat |> Seq.toList) Map.empty 0
            |> Map.toSeq
            |> Seq.groupBy snd
            |> Seq.map (fun (basinId, basins) ->
                basins
                |> Seq.map (fun ((r, s, l), i) -> l)
                |> Seq.sum)
            |> Seq.sortDescending
            |> Seq.take 3
            |> Seq.toList

        int32s |> product
