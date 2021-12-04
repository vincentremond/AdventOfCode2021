namespace AdventOfCode2021.Common

open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module String =

    let splitLines (s:string) = s.Split("\r\n")
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
    let map f (a,b) = (f a, f b)
