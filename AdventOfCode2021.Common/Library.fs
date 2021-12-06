namespace AdventOfCode2021.Common

open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module String =

    let splitLines (s: string) = s.Split("\r\n")
    let split (c: char) (s: string) = s.Split(c)

[<RequireQualifiedAccess>]
module List =
    let tryPop l =
        match l with
        | head :: tail -> Some(head, tail)
        | [] -> None

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

[<RequireQualifiedAccess>]
module Tuple =
    let map f (a, b) = (f a, f b)
    let map2 f (a1, a2) (b1, b2) = ((f a1 b1), (f a2 b2))
    let mapSnd f (a, b) = (a, f b)
    let apply2 (f1, f2) (a1, a2) (b1, b2) = ((f1 a1 b1), (f2 a2 b2))
    let transpose ((a1, a2), (b1, b2)) = ((a1, b1), (a2, b2))
    let map3 f (a1, a2) (b1, b2) (c1, c2) = ((f a1 b1 c1), (f a2 b2 c2))
    let rev (a, b) = (b, a)

[<RequireQualifiedAccess>]
module Seq =
    let countBy64 f l =
        l |> Seq.countBy f |> Seq.map (Tuple.mapSnd int64)
