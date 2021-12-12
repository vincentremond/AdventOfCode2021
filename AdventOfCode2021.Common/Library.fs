namespace AdventOfCode2021.Common

open System
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
module String =
    let splitLines (s: string) = s.Split("\r\n")
    let split (c: char) (s: string) = s.Split(c)
    let isNullOrEmpty = String.IsNullOrEmpty
    let notNullOrEmpty s = not <| isNullOrEmpty s
    let ofList (c: char list) : string = String(c |> List.toArray)
    let ofSeq (c: char seq) : string = String(c |> Seq.toArray)
    let trim (s: string) = s.Trim()
    let join (separator: string) (lines: string seq) = String.Join(separator, lines)

[<RequireQualifiedAccess>]
module Option =
    let ofList (list: List<Option<'a>>) : Option<List<'a>> =
        List.foldBack
            (fun item acc ->
                match item, acc with
                | Some value, Some list -> Some(value :: list)
                | _ -> None)
            list
            (Some [])

    let unfold (a: 'a option, b: 'b option) : ('a * 'b) option =
        match a, b with
        | Some a, Some b -> Some(a, b)
        | _ -> None

[<RequireQualifiedAccess>]
module Tuple =
    let map f (a, b) = (f a, f b)
    let map2 f (a1, a2) (b1, b2) = ((f a1 b1), (f a2 b2))
    let map3 f (a1, a2) (b1, b2) (c1, c2) = ((f a1 b1 c1), (f a2 b2 c2))
    let mapSnd f (a, b) = (a, f b)
    let apply f (a1, a2) = ((f a1), (f a2))
    let fold f (a, b) = f a b
    let apply2 (f1, f2) (a1, a2) (b1, b2) = ((f1 a1 b1), (f2 a2 b2))
    let transpose ((a1, a2), (b1, b2)) = ((a1, b1), (a2, b2))
    let rev (a, b) = (b, a)
    let unfold f1 f2 a = (f1 a, f2 a)
    let mk a b = (a, b)
    let mk3 a b c = (a, b, c)
    let ``and`` (a, b) = a && b

[<RequireQualifiedAccess>]
module Seq =
    let countBy64 f l =
        l |> Seq.countBy f |> Seq.map (Tuple.mapSnd int64)

    let range start length =
        Seq.unfold
            (fun (current, count) ->
                if count >= length then
                    None
                else
                    Some(current, (current + 1, count + 1)))
            (start, 0)

    let count f s = s |> Seq.filter f |> Seq.length

    let groupByMap groupBy map seq =
        seq
        |> Seq.groupBy groupBy
        |> Seq.map (Tuple.mapSnd map)

    let groupByFst seq =
        seq
        |> Seq.groupBy fst
        |> Seq.map (fun (a, b) -> (a, b |> Seq.map snd))

[<RequireQualifiedAccess>]
module List =
    let tryPop l =
        match l with
        | head :: tail -> Some(head, tail)
        | [] -> None

    let range start length = [ start .. 1 .. (start + length - 1) ]

    let permutations (list: 'a list) : 'a list list =
        // src: https://stackoverflow.com/questions/1526046/f-permutations
        let rec distribute e =
            function
            | [] -> [ [ e ] ]
            | x :: xs' as xs ->
                (e :: xs)
                :: [
                    for xs in distribute e xs' -> x :: xs
                ]

        let rec permute =
            function
            | [] -> [ [] ]
            | e :: xs -> List.collect (distribute e) (permute xs)

        list |> permute

    let itemR arr idx = List.item idx arr

[<RequireQualifiedAccess>]
module Array =
    let range start length =
        [| start .. 1 .. (start + length - 1) |]

    let permutations (arr: 'a array) : 'a array array =
        arr
        |> Array.toList
        |> List.permutations
        |> List.map List.toArray
        |> List.toArray

    let itemR arr idx = Array.item idx arr

    let withValue idx value arr =
        let nArr = Array.copy arr
        nArr.[idx] <- value
        nArr

    let groupByFst arr =
        arr
        |> Array.groupBy fst
        |> Array.map (fun (a, b) -> (a, b |> Array.map snd))


[<RequireQualifiedAccess>]
module Map =
    let findI m i = Map.find i m
    let tryFindI m i = Map.tryFind i m

    let addByKey map value key = Map.add key value map

[<AutoOpen>]
module Tool =
    let intersect (start1, end1) (start2, end2) : bool = (start1 <= end2) && (end1 >= start2)
    let ctoi (c: char) = (c |> int) - ('0' |> int)
    let itoc (i: int) = (i + ('0' |> int)) |> char
    let noop = ()
