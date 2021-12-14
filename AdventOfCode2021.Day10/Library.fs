namespace AdventOfCode2021.Day10

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
        let rec calculateScoreForMissing' stack score =
            match stack with
            | c :: others ->
                let newScore =
                    score * 5L
                    + match c with
                      | '(' -> 1L
                      | '[' -> 2L
                      | '{' -> 3L
                      | '<' -> 4L
                      | _ -> failwith $"Invalid char %A{c}"

                calculateScoreForMissing' others newScore
            | [] -> Some score

        calculateScoreForMissing' stack 0L

    let medianValue values =
        let index = Array.length values / 2
        let sorted = values |> Array.sort
        sorted.[index]

    let genericCalc calculateScoreForUnmatchedCloseChar calculateScoreForInvalidEndingChar resultCalc (lines: string array) =

        let isStarter =
            function
            | '('
            | '['
            | '{'
            | '<' -> true
            | _ -> false

        let getEnding =
            function
            | '(' -> Some ')'
            | '[' -> Some ']'
            | '{' -> Some '}'
            | '<' -> Some '>'
            | _ -> None

        let rec explore notClosedStarts charsToExplore =
            match charsToExplore with
            | c :: otherChars ->
                match isStarter c with
                | true -> explore (c :: notClosedStarts) otherChars
                | false ->
                    match notClosedStarts with
                    | start :: otherStarts ->
                        let ending = getEnding start |> Option.get

                        if ending = c then
                            explore otherStarts otherChars
                        else
                            calculateScoreForInvalidEndingChar c
                    | [] -> failwith "Trying to close more than what was opened"
            | [] -> calculateScoreForUnmatchedCloseChar notClosedStarts

        lines
        |> Array.choose (List.ofSeq >> (explore []))
        |> resultCalc

    let part1 =
        genericCalc none calculateScoreForInvalid Array.sum

    let part2 =
        genericCalc calculateScoreForMissing none medianValue
