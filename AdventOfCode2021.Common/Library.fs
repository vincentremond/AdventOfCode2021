namespace AdventOfCode2021.Common

open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module String =

    let splitLines s = Regex.Split(s, "[\r\n]+")
