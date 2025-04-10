﻿namespace AdventOfCode2021.Day10

open AdventOfCode2021.Common
open Microsoft.FSharp.Core

module Solution =

    let none _ = None

    let calculateScoreForInvalid =
        function
        | ')' -> Some 3
        | ']' -> Some 57
        | '}' -> Some 1197
        | '>' -> Some 25137
        | _ -> None

    let calculateScoreForMissing stack =

        stack
        |> List.fold
            (fun score c ->
                score * 5L
                + match c with
                  | '(' -> 1L
                  | '[' -> 2L
                  | '{' -> 3L
                  | '<' -> 4L
                  | _ -> failwith "Invalid character in stack"
            )
            0L
        |> Some

    let medianValue values =
        let index = Array.length values / 2
        let sorted = values |> Array.sort
        sorted.[index]

    let genericCalc
        calculateScoreForUnmatchedCloseChar
        calculateScoreForInvalidEndingChar
        resultCalc
        (lines: string array)
        =

        let rec explore notClosedStarts charsToExplore =
            match charsToExplore with
            | currentChar :: otherChars ->
                match currentChar with
                | '('
                | '['
                | '{'
                | '<' -> explore (currentChar :: notClosedStarts) otherChars
                | _ ->
                    match notClosedStarts with
                    | firstNotClosedStart :: otherStarts ->
                        match firstNotClosedStart, currentChar with
                        | '(', ')'
                        | '[', ']'
                        | '{', '}'
                        | '<', '>' -> explore otherStarts otherChars
                        | _ -> calculateScoreForInvalidEndingChar currentChar
                    | [] -> failwith "Trying to close more than what was opened"
            | [] -> calculateScoreForUnmatchedCloseChar notClosedStarts

        lines |> Array.choose (List.ofSeq >> (explore [])) |> resultCalc

    let part1 = genericCalc none calculateScoreForInvalid Array.sum

    let part2 = genericCalc calculateScoreForMissing none medianValue
