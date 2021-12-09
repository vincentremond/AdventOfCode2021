namespace AdventOfCode2021.Day08

open System
open System.Text.RegularExpressions
open AdventOfCode2021.Common

type Wiring = char list

type ParseResult =
    {
        Wirings: Wiring list
        Displays: Wiring list
    }

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
        |> Seq.map
            (fun l ->
                let ``match`` = l |> LineRegex().TypedMatch

                {
                    Wirings = ``match``.Wiring.Captures |> mapCaptures
                    Displays = ``match``.Display.Captures |> mapCaptures
                })
        |> Seq.toList

module Solution =

    let part1 lines =

        let uniqueDigitLengths = [ 2; 3; 4; 7 ]

        lines
        |> Parser.parseInput
        |> Seq.map
            (fun parseResult ->
                parseResult.Displays
                |> Seq.count (fun (d: Wiring) -> List.contains (List.length d) uniqueDigitLengths))
        |> Seq.sum

    let part2 lines =

        let mapping =
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

            let charMap =
                w
                |> Seq.concat
                |> Seq.countBy id
                |> Seq.map (fun (ch, ct) -> Option.map2 Tuple.mk (Some ch) (mapping |> Map.tryFind ct))
                |> Seq.choose id
                |> Map.ofSeq

            let c =
                w
                |> Seq.find (fun w -> List.length w = 2)
                |> Seq.choose
                    (fun c ->
                        match c |> Map.tryFindI charMap with
                        | Some _ -> None
                        | None -> Some c)
                |> Seq.head

            let charMap = charMap |> Map.add c 'c'

            let a =
                w
                |> Seq.find (fun w -> List.length w = 3)
                |> Seq.choose
                    (fun c ->
                        match c |> Map.tryFindI charMap with
                        | Some _ -> None
                        | None -> Some c)
                |> Seq.head

            let charMap = charMap |> Map.add a 'a'

            let d =
                w
                |> Seq.find (fun w -> List.length w = 4)
                |> Seq.choose
                    (fun c ->
                        match c |> Map.tryFindI charMap with
                        | Some _ -> None
                        | None -> Some c)
                |> Seq.head

            let charMap = charMap |> Map.add d 'd'

            let g =
                w
                |> Seq.find (fun w -> List.length w = 7)
                |> Seq.choose
                    (fun c ->
                        match c |> Map.tryFindI charMap with
                        | Some _ -> None
                        | None -> Some c)
                |> Seq.head

            let charMap = charMap |> Map.add g 'g'
            charMap

        let digits =
            [|
                @"abcefg" (*     0  |              |  *)
                @"cf" (*         1  |    A A A     |  *)
                @"acdeg" (*      2  |  B       C   |  *)
                @"acdfg" (*      3  |  B       C   |  *)
                @"bcdf" (*       4  |  B       C   |  *)
                @"abdfg" (*      5  |    D D D     |  *)
                @"abdefg" (*     6  |  E       F   |  *)
                @"acf" (*        7  |  E       F   |  *)
                @"abcdefg" (*    8  |  E       F   |  *)
                @"abcdfg" (*     9  |    G G G     |  *)
            |]
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
                |> List.map
                    (fun display ->
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


//
//        let rec pow v x =
//            match x with
//            | 0 -> 1
//            | 1 -> x
//            | xx -> xx * (pow v (x - 1))
//
//        let digits =
//            [|
//                @"abcefg" (*     0  |              |  *)
//                @"cf" (*         1  |    A A A     |  *)
//                @"acdeg" (*      2  |  B       C   |  *)
//                @"acdfg" (*      3  |  B       C   |  *)
//                @"bcdf" (*       4  |  B       C   |  *)
//                @"abdfg" (*      5  |    D D D     |  *)
//                @"abdefg" (*     6  |  E       F   |  *)
//                @"acf" (*        7  |  E       F   |  *)
//                @"abcdefg" (*    8  |  E       F   |  *)
//                @"abcdfg" (*     9  |    G G G     |  *)
//            |]
//
//        let strToDigit =
//            digits
//            |> Array.mapi (Tuple.mk)
//            |> Array.map Tuple.rev
//            |> Map.ofSeq
//
//
//        let validPositions =
//            digits
//            |> Seq.map Parser.mapWiring
//            |> Parser.mapWirings
//
//
//        let permutations =
//            [ 2; 3; 4; 5; 6; 7 ]
//            |> List.map (List.range 0 >> List.permutations)
//
//        let rec isValid (mapper: Mapper) (map: char list, wiring: char list) : Mapper =
//            match mapper with
//            | Incomplete (chars, found, count) ->
//                match map, wiring with
//                | m :: om, w :: ow ->
//                    let iM = ctoi m
//                    let iW = ctoi w
//
//                    match chars.[iM], found.[iW] with
//                    | _, true -> Invalid
//                    | None, _ ->
//                        let newMapper = mapper.WithValue(iM, w)
//                        isValid newMapper (om, ow)
//                    | Some e, _ when e = w -> isValid mapper (om, ow)
//                    | Some _, _ -> Invalid
//                | [], [] -> mapper
//                | _ -> failwith "Error"
//            | _ -> mapper
//
//        let rec exploreCouplings c currentMapper =
//            match c with
//            | (wirings, possibilities, permutations) :: otherCouplings ->
//                let map =
//                    List.allPairs possibilities permutations
//                    |> List.map (fun (pos, per) -> per |> List.map (pos |> List.itemR))
//
//                let possibleMappings = List.allPairs map wirings
//
//                exploreMappings possibleMappings otherCouplings currentMapper
//
//
//            | [] -> Invalid
//
//        and exploreMappings possibleMappings otherCouplings currentMapper =
//            match possibleMappings with
//            | mapping :: otherMappings ->
//                let isValid = isValid currentMapper mapping
//
//                match isValid with
//                | Complete _ -> isValid
//                | Invalid -> exploreMappings otherMappings otherCouplings currentMapper
//                | Incomplete _ -> exploreCouplings otherCouplings isValid
//            | [] -> exploreCouplings otherCouplings currentMapper
//
//
//        let solve display =
//
//            let couplingBySize =
//                List.map3 Tuple.mk3 display.Wirings validPositions permutations
//                |> Seq.toList
//
//
//            let mapper = Mapper.mk 7
//            exploreCouplings couplingBySize mapper
//
//        let mappers = input |> List.map solve
//        let xx = List.zip input mappers
//
//        let getDigits (parsed, mapper) =
//            match mapper with
//            | Complete mapping ->
//                let mapIndexed =
//                    mapping
//                    |> Array.mapi (fun index item -> (item, (itoc index)))
//                    |> Map.ofSeq
//
//                parsed.Displays
//                |> List.map
//                    (fun wiring ->
//                        wiring
//                        |> List.map (fun c -> mapIndexed |> Map.find c))
//                |> List.map (fun wiring -> String.ofList wiring)
//                |> List.rev
//                |> List.mapi
//                    (fun index wiring ->
//                        let digit = strToDigit |> Map.find wiring
//                        let powerOfTen = pow 10 index
//                        digit * powerOfTen)
//                |> List.sum
//
//            | _ -> failwith "todo"
//
//
//
//        xx |> List.map getDigits |> List.sum

//        let getPermutations i =
//            (i, i |> List.range 0 |> List.permutations)
//
//        let permutations =
//            validPositions
//            |> Seq.choose
//                (function
//                | 7, _ -> None
//                | x, _ -> Some(getPermutations x))
//            |> Map.ofSeq
//
//        let rec exploreSolutions ff (validPositions: (int * Wiring) list) mapper =
//            match validPositions with
//            | (size, wiring) :: otherValidPositions ->
//                let possiblePermutationsForSize = permutations |> Map.find size
//
//                let xxx =
//                    possiblePermutationsForSize
//                    |> List.map (fun permutation -> permutation |> List.map (fun idx -> wiring.[idx]))
//
//                failwith "todo"
//            | _ -> failwith "todo"
//
//        let explore
//
//        let rec exploreWirings wirings mapper =
//            match wirings with
//            | wiring :: otherValidPositions ->
//                let size = wiring |> List.length
//                let possiblePermutationsForSize = permutations |> Map.find size
//                let
//
//                let xxx =
//                    possiblePermutationsForSize
//                    |> List.map (fun permutation -> permutation |> List.map (fun idx -> wiring.[idx]))
//
//                failwith "todo"
//            | _ -> failwith "todo"
//
//        let solveForDisplay display  =
//            let mapper = [| for _ in 1 .. 7 -> None |]
//
//            exploreWirings display.Wirings validPositions mapper
//
//
//        input |> Seq.head |> solveForDisplay
