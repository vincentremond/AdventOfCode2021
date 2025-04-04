﻿namespace AdventOfCode2021.Common

open System
open System.Text.RegularExpressions
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
    let empty = String.Empty

[<RequireQualifiedAccess>]
module Option =
    let ofList (list: List<Option<'a>>) : Option<List<'a>> =
        List.foldBack
            (fun item acc ->
                match item, acc with
                | Some value, Some list -> Some(value :: list)
                | _ -> None
            )
            list
            (Some [])

    let unfold (a: 'a option, b: 'b option) : ('a * 'b) option =
        match a, b with
        | Some a, Some b -> Some(a, b)
        | _ -> None

    let unwrap (t: ('a * 'b) option) : 'a option * 'b option =
        match t with
        | Some(a, b) -> (Some a, Some b)
        | _ -> (None, None)

    let merge (merger: 'a -> 'a -> 'a) (a1: 'a option, a2: 'a option) =
        match a1, a2 with
        | Some v1, Some v2 -> Some(merger v1 v2)
        | Some v1, None -> Some v1
        | None, Some v2 -> Some v2
        | None, None -> None

[<RequireQualifiedAccess>]
module Tuple =
    let map f (a, b) = (f a, f b)
    let map2 f (x1, x2) (y1, y2) = ((f x1 y1), (f x2 y2))
    let map3 f (a1, a2) (b1, b2) (c1, c2) = ((f a1 b1 c1), (f a2 b2 c2))
    let mapSnd f (a, b) = (a, f b)
    let apply f (a1, a2) = ((f a1), (f a2))
    let fold f (a, b) = f a b
    let apply2f1t (f1, f2) (a1, a2) = ((f1 a1), (f2 a2))
    let apply2f2t (f1, f2) (a1, a2) (b1, b2) = ((f1 a1 b1), (f2 a2 b2))
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
                    Some(current, (current + 1, count + 1))
            )
            (start, 0)

    let count f s = s |> Seq.filter f |> Seq.length

    let groupByMap groupBy map seq =
        seq |> Seq.groupBy groupBy |> Seq.map (Tuple.mapSnd map)

    let groupByFst seq =
        seq |> Seq.groupBy fst |> Seq.map (fun (a, b) -> (a, b |> Seq.map snd))

[<RequireQualifiedAccess>]
module List =
    let tryPop l =
        match l with
        | head :: tail -> Some(head, tail)
        | [] -> None

    let pop l = (l |> List.head, l |> List.tail)

    let maybePop l =
        match l with
        | head :: tail -> Some(head), tail
        | [] -> None, []

    let range start length = [ start..1 .. (start + length - 1) ]

    let permutations (list: 'a list) : 'a list list =
        // src: https://stackoverflow.com/questions/1526046/f-permutations
        let rec distribute e =
            function
            | [] -> [ [ e ] ]
            | x :: xs' as xs -> (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]

        let rec permute =
            function
            | [] -> [ [] ]
            | e :: xs -> List.collect (distribute e) (permute xs)

        list |> permute

    let itemR arr idx = List.item idx arr

    let product = List.fold (fun state item -> item * state) 1UL

[<RequireQualifiedAccess>]
module Array =
    let range start length = [| start..1 .. (start + length - 1) |]

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
        arr |> Array.groupBy fst |> Array.map (fun (a, b) -> (a, b |> Array.map snd))

    let groupByFstAndMap (map: 'b array -> 'c) (arr: ('a * 'b) array) : ('a * 'c) array =
        arr
        |> Array.groupBy fst
        |> Array.map (fun (a, arr) -> (a, arr |> (Array.map snd >> map)))

[<RequireQualifiedAccess>]
module Map =
    let findI m i = Map.find i m
    let tryFindI m i = Map.tryFind i m

    let tryFindOrDefault k v m =
        Map.tryFind k m |> Option.defaultValue v

    let addByKey map value key = Map.add key value map

[<AutoOpen>]
module Tool =
    let intersect (start1, end1) (start2, end2) : bool = (start1 <= end2) && (end1 >= start2)
    let ctoi (c: char) = (c |> int) - ('0' |> int)

    let itoc (i: int) =
        if i > 9 || i < 0 then
            raise (ArgumentOutOfRangeException(nameof (i), i, "Not a single digit value"))

        (i + ('0' |> int)) |> char

    let noop = ()

[<AutoOpen>]
module ActivePatterns =
    let (|IsMatch|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then Some m else None

module Direction =
    let move max directions startingPoint =
        let mapDir max x diff =
            let n = x + diff

            if n < 0 then None
            else if n > max then None
            else Some n

        directions |> Seq.choose (Tuple.map3 mapDir max startingPoint >> Option.unfold)
