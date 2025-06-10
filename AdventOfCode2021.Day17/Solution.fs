namespace AdventOfCode2021.Day17

open System
open System.Text.RegularExpressions

[<Measure>]
type Step

[<Measure>]
type HPos

[<Measure>]
type HSpeed = HPos / Step

[<Measure>]
type VPos

[<Measure>]
type VSpeed = VPos / Step

type Target = {
    MinX: int<HPos>
    MaxX: int<HPos>
    MinY: int<VPos>
    MaxY: int<VPos>
}

type Result = {
    Steps: int<Step>
    Peak: int<VPos>
    InitialHSpeed: int<HSpeed>
    InitialVSpeed: int<VSpeed>
    PosX: int<HPos>
    PosY: int<VPos>
}

module Solution =

    let parseInput s =
        let regex =
            "^target area: x=(?<MinX>-?\d+)\.\.(?<MaxX>-?\d+), y=(?<MinY>-?\d+)..(?<MaxY>-?\d+)$"
            |> Regex

        let m = s |> regex.Match

        let getV (s: string) =
            m.Groups.[s].Value |> int |> LanguagePrimitives.Int32WithMeasure

        {
            MinX = getV "MinX"
            MaxX = getV "MaxX"
            MinY = getV "MinY"
            MaxY = getV "MaxY"
        }

    let incVPos (pos: int<VPos>) (vX: int<VSpeed>) = (pos + (vX * 1<Step>))

    let calcHPosAtStep (initialHSpeed: int<HSpeed>) (step: int<Step>) : int<HPos> =
        let step = step |> int
        let initialHSpeed = initialHSpeed |> int

        let step = min step initialHSpeed

        step * initialHSpeed - ((step * (step - 1)) / 2)
        |> LanguagePrimitives.Int32WithMeasure

    let getMax _ possibleVSpeeds =
        possibleVSpeeds |> Seq.map (fun (_, vPos: int<VPos>, _) -> vPos) |> Seq.max

    let getDifferentCount possibleHSpeeds possibleVSpeeds =
        Seq.allPairs possibleHSpeeds possibleVSpeeds
        |> Seq.choose (fun ((hStep, hSpeed, verticalDrop), (vStep, _, vSpeed)) ->
            if hStep = vStep then
                Some(hSpeed, vSpeed)
            else if hStep < vStep && verticalDrop then
                Some(hSpeed, vSpeed)
            else
                None
        )
        |> Seq.distinct
        |> Seq.toArray
        |> Array.length

    let calc getResult input =
        let target = parseInput input

        let possibleHSpeeds =
            [ 1<HSpeed> .. 1<HSpeed> .. (target.MaxX / 1<Step>) ]
            |> List.map (fun initialHSpeed ->
                let unfold
                    (currentStep: int<Step> option)
                    : ((int<Step> * int<HSpeed> * bool) option * int<Step> option) option =
                    match currentStep with
                    | None -> None
                    | Some currentStep ->
                        let x = calcHPosAtStep initialHSpeed currentStep

                        let isVerticalDrop = ((currentStep |> int) >= (initialHSpeed |> int))

                        let value =
                            if x <= target.MaxX && x >= target.MinX then
                                Some(currentStep, initialHSpeed, isVerticalDrop)
                            else
                                None

                        let nextStep =
                            if x > target.MaxX then None
                            else if isVerticalDrop then None
                            else Some(currentStep + 1<Step>)

                        Some(value, nextStep)

                let stepsInRange = List.unfold unfold (1<Step> |> Some) |> List.choose id

                stepsInRange
            )
            |> List.concat

        let getPossibleVSpeeds (speed: int<VSpeed>) : (int<Step> * int<VPos> * int<VSpeed>) list =
            let rec get' step pos spd currMax acc =
                let pos = incVPos pos spd

                let currMax =
                    match currMax with
                    | None -> Some pos
                    | Some m -> Some(max pos m)

                let next = get' (step + 1<Step>) pos (spd - 1<VSpeed>) currMax

                if pos >= target.MinY && pos <= target.MaxY then
                    next ((step, currMax |> Option.get, speed) :: acc)
                else if pos >= target.MinY then
                    next acc
                else
                    acc

            get' 1<Step> 0<VPos> speed None []

        let possibleVSpeeds =
            [ target.MinY .. 1<VPos> .. ((target.MinY |> abs) - 1<VPos>) ]
            |> List.map (fun possibleVSpeed ->
                let possibleVSpeed = (possibleVSpeed |> int) * 1<VSpeed>
                getPossibleVSpeeds possibleVSpeed
            )
            |> List.concat

        getResult possibleHSpeeds possibleVSpeeds

    let part1 = calc getMax
    let part2 = calc getDifferentCount
