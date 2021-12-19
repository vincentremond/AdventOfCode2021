namespace AdventOfCode2021.Day18

open System
open AdventOfCode2021.Common

[<Measure>]
type Level

[<Measure>]
type Value

type Number = int<Value> * int<Level>

type SplitStatus =
    | Ok
    | MustReduce

type ReduceStatus =
    | Ok
    | MustSplit

type Nbr =
    | Raw of int<Value>
    | Grp of Nbr * Nbr
    override this.ToString() =
        let rec displayNumber' n =
            seq {
                match n with
                | Raw v -> yield (v |> int |> itoc)
                | Grp (left, right) ->
                    yield '['
                    yield! displayNumber' left
                    yield ','
                    yield! displayNumber' right
                    yield ']'
            }

        displayNumber' this |> Seq.toArray |> String

type SearchFor =
    | Left
    | Right


module Solution =

    let transpose (nbr: Number list) : Nbr =
        let rec regroup numbers (previous: (int<Level> * Nbr) option) results regroupLevel =
            match numbers with
            | curr :: others ->
                match previous with
                | None -> regroup others (Some curr) results regroupLevel
                | Some (prevL, prevV as prev) ->
                    let currL, currV = curr

                    let results, currentAsPrevious =
                        if prevL = regroupLevel && currL = regroupLevel then
                            let grpL = currL + 1<Level>
                            (((grpL, Grp(prevV, currV)) :: results), None)
                        else
                            ((prev :: results), Some curr)

                    regroup others currentAsPrevious results regroupLevel
            | [] ->
                let result =
                    match previous with
                    | Some p -> (p :: results)
                    | None -> results

                match result with
                | [ one ] -> one |> snd
                | [] -> failwith "Empty ? Nani ?!?"
                | items -> regroup (items |> List.rev) None [] (regroupLevel + 1<Level>)

        regroup (nbr |> List.map (fun (v, l) -> (l, Raw v))) None [] 0<Level>


    let display (nbr: Number list) : string = nbr |> transpose |> string

    let parseNumber (str: string) =
        let discard c lst =
            match lst with
            | x :: o ->
                if x = c then
                    o
                else
                    failwith $"Unexpected char '{x}'."
            | [] -> failwith $"Unexpected EOL."

        let rec parseNumber' (level: int<Level>) acc (numbers: char list) : Number list * char list =
            match numbers with
            | c :: lst ->
                match c with
                | d when d >= '0' && d <= '9' ->
                    let value =
                        c |> ctoi |> LanguagePrimitives.Int32WithMeasure

                    ((value, level) :: acc, lst)
                | '[' ->
                    let acc, lst =
                        lst |> parseNumber' (level - 1<Level>) acc

                    let lst = lst |> discard ','

                    let acc, lst =
                        lst |> parseNumber' (level - 1<Level>) acc

                    let lst = lst |> discard ']'
                    (acc, lst)
                | x -> failwith $"Unexpected char '{x}'."

            | [] -> (acc, [])

        str
        |> Seq.toList
        |> (parseNumber' 5<Level> [])
        |> fst
        |> List.rev

    let getMagnitude (n: Nbr) : int =
        let rec get' (n: Nbr) : int =
            match n with
            | Raw v -> v |> int
            | Grp (a, b) ->
                (a, b)
                |> Tuple.map get'
                |> Tuple.map2 (*) (3, 2)
                |> Tuple.fold (+)

        get' n

    let rec reduce (levelMax: int<Level>) (nbr: Number list) =

        let shouldReduce (_, bL) (_, cL) =
            bL < levelMax
            && cL < levelMax

        let rec reduce'
            (left: Number list)
            (right: Number list)
            (a: Number option)
            (b: Number)
            (c: Number)
            (d: Number option)
            (status: ReduceStatus)
            : (ReduceStatus * Number list) =
            if shouldReduce b c then
                let newLeft, status =
                    match a with
                    | Some (aV, aL) ->
                        let foo = aV + fst b

                        let status =
                            if foo > 9<Value> then
                                ReduceStatus.MustSplit
                            else
                                status

                        (foo, aL) :: left, status
                    | None -> left, status

                let replacement = (0<Value>, levelMax)

                match d with
                | Some (dV, dL) ->

                    let newA = Some(replacement)
                    let newBValue = fst c + dV
                    let newB = (newBValue, dL)
                    let newC, newRight = List.tryPop right
                    let newD, newRight = List.tryPop newRight

                    let status =
                        if newBValue > 9<Value> then
                            ReduceStatus.MustSplit
                        else
                            status

                    match newC with
                    | Some newC -> reduce' newLeft newRight newA newB newC newD status
                    | None -> status, ((newB :: (replacement :: newLeft)) |> List.rev)
                | None -> status, ((replacement :: newLeft) |> List.rev)
            else
                let newLeft =
                    match a with
                    | Some a -> (a :: left)
                    | None -> left

                match d with
                | Some d ->
                    let newA = Some b
                    let newB = c
                    let newC = d
                    let newD, newRight = List.tryPop right
                    reduce' newLeft newRight newA newB newC newD status
                | None -> status, ((c :: (b :: newLeft)) |> List.rev)


        let a = None
        let b, nbr = List.pop nbr
        let c, nbr = List.pop nbr
        let d, nbr = List.tryPop nbr

        match reduce' [] nbr a b c d ReduceStatus.Ok with
        | ReduceStatus.MustSplit, s -> split levelMax s
        | ReduceStatus.Ok, s -> s

    and split (levelMax: int<Level>) (nbr: Number list) =
        let rec split' (nbr: Number list) (acc: Number list) (status: SplitStatus) : SplitStatus * Number list =
            match nbr with
            | value, level as x :: others ->
                if value > 9<Value> then
                    let a = value / 2
                    let b = (value / 2 + (value % 2<Value>))
                    let l = level - 1<Level>

                    let status =
                        match status, l < levelMax with
                        | SplitStatus.Ok, false -> SplitStatus.Ok
                        | _ -> MustReduce

                    split' others ((b, l) :: ((a, l) :: acc)) status
                else
                    split' others (x :: acc) status
            | [] -> status, (acc |> List.rev)

        match split' nbr [] SplitStatus.Ok with
        | SplitStatus.MustReduce, s -> reduce levelMax s
        | SplitStatus.Ok, s -> s

    let add a b =
        (a @ b)
        |> List.map (fun (v, l) -> (v, l - 1<Level>))
        |> reduce 1<Level>

    let sum (lst: Number list list) : Number list =
        let rec add' (acc: Number list option) (lst: Number list list) : Number list =
            match acc, lst with
            | Some curr, toAdd :: others ->
                let summed = add curr toAdd

                add' (Some summed) others
            | Some curr, [] -> curr
            | None, toAdd :: others -> add' (Some toAdd) others
            | None, [] -> []

        add' None lst


    let calc inputs = -1

    let part1 = calc
    let part2 = calc
