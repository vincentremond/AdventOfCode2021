namespace AdventOfCode2021.Common

open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module String =

    let splitLines s = Regex.Split(s, "[\r\n]+")

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
