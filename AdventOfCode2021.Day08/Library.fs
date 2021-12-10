namespace AdventOfCode2021.Day08

open System
open System.Text.RegularExpressions
open AdventOfCode2021.Common

type Wiring = char list

type ParseResult =
    { Wirings: Wiring list
      Displays: Wiring list }

module Parser =
    open FSharp.Text.RegexProvider
    type LineRegex = Regex< @"^((?<Wiring>\w+) )+\|( (?<Display>\w+))+$" >

    let mapWiring (s: string) : Wiring = s |> Seq.sort |> Seq.toList

    let mapWirings (w: Wiring seq) : Wiring list list =
        w
        |> Seq.groupBy List.length
        |> Seq.sortBy fst
        |> Seq.map (snd >> Seq.toList)
        |> Seq.toList

    let mapCaptures c =
        c
        |> Seq.map (fun (c: Capture) -> c.Value)
        |> Seq.map mapWiring
        |> Seq.toList

    let parseInput lines =


        lines
        |> Seq.map (fun l ->
            let ``match`` = l |> LineRegex().TypedMatch

            { Wirings = ``match``.Wiring.Captures |> mapCaptures
              Displays = ``match``.Display.Captures |> mapCaptures })
        |> Seq.toList

module Solution =

    let part1 lines =

        let uniqueDigitLengths = [ 2; 3; 4; 7 ]

        lines
        |> Parser.parseInput
        |> Seq.map (fun parseResult ->
            parseResult.Displays
            |> Seq.count (fun (d: Wiring) -> List.contains (List.length d) uniqueDigitLengths))
        |> Seq.sum

    let part2 lines =

        let barCountMapping =
            [|
               //8, 'a'
               6, 'b'
               //8, 'c'
               //7, 'd'
               4, 'e'
               9, 'f'
               //7, 'g'
               |]
            |> Map.ofSeq

        let solveOne xx =
            let w = xx.Wirings

            let deduceFromSize size charToDeduce charMap =
                w
                |> Seq.find (fun w -> List.length w = size)
                |> Seq.choose (fun c ->
                    match c |> Map.tryFindI charMap with
                    | Some _ -> None
                    | None -> Some c)
                |> Seq.exactlyOne
                |> Map.addByKey charMap charToDeduce

            w
            |> Seq.concat
            |> Seq.countBy id
            |> Seq.map (fun (ch, ct) -> Option.map2 Tuple.mk (Some ch) (barCountMapping |> Map.tryFind ct))
            |> Seq.choose id
            |> Map.ofSeq
            |> deduceFromSize 2 'c'
            |> deduceFromSize 3 'a'
            |> deduceFromSize 4 'd'
            |> deduceFromSize 7 'g'

        let digits =
            [| @"abcefg" (*     0  |              |  *)
               @"cf" (*         1  |    A A A     |  *)
               @"acdeg" (*      2  |  B       C   |  *)
               @"acdfg" (*      3  |  B       C   |  *)
               @"bcdf" (*       4  |  B       C   |  *)
               @"abdfg" (*      5  |    D D D     |  *)
               @"abdefg" (*     6  |  E       F   |  *)
               @"acf" (*        7  |  E       F   |  *)
               @"abcdefg" (*    8  |  E       F   |  *)
               @"abcdfg" (*     9  |    G G G     |  *)  |]
            |> Array.mapi (Tuple.mk)
            |> Array.map Tuple.rev
            |> Map.ofSeq

        let rec pow v x =
            match x with
            | 0 -> 1
            | 1 -> x
            | xx -> xx * (pow v (x - 1))

        let getResult (parseResult: ParseResult, mapping: Map<char, char>) : int =
            let map =
                parseResult.Displays
                |> List.map (fun display ->
                    display
                    |> List.map (mapping |> Map.findI)
                    |> List.sort
                    |> String.ofList)

            map
            |> List.map (digits |> Map.findI)
            |> List.rev
            |> List.mapi (fun p value -> (Math.Pow(10., p |> float) |> int) * value)
            |> List.sum

        lines
        |> Parser.parseInput
        |> List.map (fun x -> (x, solveOne x))
        |> List.map getResult
        |> List.sum
