namespace AdventOfCode2021.Day17

open System
open System.Text.RegularExpressions
open AdventOfCode2021.Day17

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

type Target =
    {
        MinX: int<HPos>
        MaxX: int<HPos>
        MinY: int<VPos>
        MaxY: int<VPos>
    }

type Result =
    {
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
            m.Groups.[s].Value
            |> int
            |> LanguagePrimitives.Int32WithMeasure

        {
            MinX = getV "MinX"
            MaxX = getV "MaxX"
            MinY = getV "MinY"
            MaxY = getV "MaxY"
        }

    let incHPos (pos: int<HPos>) (vX: int<HSpeed>) = (pos + (vX * 1<Step>))
    let incVPos (pos: int<VPos>) (vX: int<VSpeed>) = (pos + (vX * 1<Step>))

    let tryResolveVX (minX: int<HPos>) (maxX: int<HPos>) (vX: int<HSpeed>) =
        let rec step (pos: int<HPos>) (idx: int<Step>) (vX': int<HSpeed>) =
            if pos >= minX && pos <= maxX then
                Some(idx, vX)
            else if pos > maxX then
                None
            else if vX' = 0<HSpeed> then
                None
            else
                step (incHPos pos vX') (idx + 1<Step>) (vX' - 1<HSpeed>)

        step (incHPos 0<HPos> vX) 1<Step> vX

    let getPossibleVX (minX: int<HPos>) (maxX: int<HPos>) : (int<Step> * int<HSpeed>) list =

        let maxSpeed = maxX / 1<Step>

        [ maxSpeed .. -1<HSpeed> .. 1<HSpeed> ]
        |> List.choose (tryResolveVX minX maxX)

    let calcInitialVerticalSpeed (p: int<VPos>) (s: int<Step>) =
        let p = p |> decimal
        let s = s |> decimal
        (p + ((s * (s - 1M)) / 2M)) / s

    let searchMaxVerticalSpeed (minY: int<VPos>) (maxY: int<VPos>) (steps: int<Step>) : (int<VSpeed> * int<VSpeed>) option =
        let maxVSpeed =
            calcInitialVerticalSpeed maxY steps
            |> Math.Floor
            |> int

        let minVSpeed =
            calcInitialVerticalSpeed minY steps
            |> Math.Ceiling
            |> int

        if minVSpeed <= maxVSpeed then
            Some(maxVSpeed * 1<VSpeed>, minVSpeed * 1<VSpeed>)
        else
            None

    let calcHPosAtStep (initialHSpeed: int<HSpeed>) (step: int<Step>) : int<HPos> =
        let step = step |> int
        let initialHSpeed = initialHSpeed |> int

        let step = min step initialHSpeed

        step * initialHSpeed - ((step * (step - 1)) / 2)
        |> LanguagePrimitives.Int32WithMeasure

    let calcVPosAtStep (initialVSpeed: int<VSpeed>) (step: int<Step>) : int<VPos> =
        let step = step |> int
        let initialVSpeed = initialVSpeed |> int

        step * initialVSpeed - ((step * (step - 1)) / 2)
        |> LanguagePrimitives.Int32WithMeasure

    let findPeak initialVSpeed maxSteps =
        [ 1<Step> .. 1<Step> .. maxSteps ]
        |> List.map (calcVPosAtStep initialVSpeed)
        |> List.max

    let getPossibleVSpeeds target =
        let rec possibleVSpeeds' currentStep acc =
            match searchMaxVerticalSpeed target.MinY target.MaxY currentStep with
            | None -> acc
            | Some (min, max) -> possibleVSpeeds' (currentStep + 1<Step>) ((currentStep, (min, max)) :: acc)

        possibleVSpeeds' 1<Step> []

    let calc input =
        let target = parseInput input

        let possibleHSpeeds =
            [
                1<HSpeed> .. 1<HSpeed> .. (target.MaxX / 1<Step>)
            ]
            |> List.map
                (fun initialHSpeed ->
                    let unfold (currentStep: int<Step> option) : ((int<Step> * int<HSpeed> * bool) option * int<Step> option) option =
                        match currentStep with
                        | None -> None
                        | Some currentStep ->
                            let x = calcHPosAtStep initialHSpeed currentStep

                            let isVerticalDrop =
                                ((currentStep |> int) >= (initialHSpeed |> int))

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

                    let stepsInRange =
                        List.unfold unfold (1<Step> |> Some)
                        |> List.choose id

                    stepsInRange)
            |> List.concat

        let possibleVSpeed =
            [|
                target.MinY .. 1<VPos> .. ((target.MinY |> abs) - 1<VPos>)
            |] |> List.map (fun possibleVSpeed -> )





        possibleHSpeeds
    //
//        let validValues =
//            [ 2 .. (target.MaxX |> int) ]
//            |> List.map
//                (fun force ->
//                    [ 1 .. (force - 1) ]
//                    |> List.map
//                        (fun hSpeed ->
//                            let vSpeed = (force - hSpeed) * 1<VSpeed>
//                            let hSpeed = hSpeed * 1<HSpeed>
//
//                            //let unfold (state:'state) : ('t * 'state) option =
//                            let unfold (currentStep: int<Step>) : ((int<Step> * int<VPos>) option * int<Step>) option =
//                                let x = calcVPosAtStep vSpeed currentStep
//
//                                if x <= target.MaxY && x >= target.MinY then
//                                    Some(Some(currentStep, x), currentStep + 1<Step>)
//                                else if x < target.MinY then
//                                    None
//                                else
//                                    Some(None, currentStep + 1<Step>)
//
//                            let stepsInRange =
//                                List.unfold unfold 1<Step> |> List.choose id
//
//
//                            let choose =
//                                stepsInRange
//                                |> List.map
//                                    (fun (step, vPos) ->
//                                        let hPos = calcHPosAtStep hSpeed step
//
//                                        if hPos >= target.MinX && hPos <= target.MaxX then
//                                            let peak = findPeak vSpeed step
//
//
//                                            Some
//                                                {
//                                                    Steps = step
//                                                    Peak = peak
//                                                    InitialHSpeed = hSpeed
//                                                    InitialVSpeed = vSpeed
//                                                    PosX = hPos
//                                                    PosY = vPos
//
//                                                }
//                                        else
//                                            None)
//                                |> List.choose id
//
//                            choose)
//
//                    )
//            |> List.concat
//            |> List.concat
//            |> List.sortByDescending (fun r -> r.Peak)
//
//        let x = validValues |> List.head
//        x.Peak

    let part1 = calc
    let part2 = calc
