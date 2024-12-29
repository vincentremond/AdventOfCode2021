namespace AdventOfCode2021.Day02

open FSharp.Text.RegexProvider

module Solution =

    type Line = Regex<"^(?<Direction>up|down|forward) (?<Distance>\d+)$">

    type Direction =
        | Forward of int
        | Down of int
        | Up of int

        static member mk s i =
            i
            |> match s with
               | "forward" -> Forward
               | "down" -> Down
               | "up" -> Up
               | _ -> failwith $"Unmatched direction {s}"

    type Result = {
        Horizontal: int
        Vertical: int
        Aim: int
    } with

        static member calcOutput r = r.Horizontal * r.Vertical

        static member blank = {
            Vertical = 0
            Horizontal = 0
            Aim = 0
        }

    let calculateNewPosition (s: Result) (currentItem: Direction) =
        match currentItem with
        | Down d -> { s with Vertical = s.Vertical + d }
        | Up u -> { s with Vertical = s.Vertical - u }
        | Forward f -> { s with Horizontal = s.Horizontal + f }

    let calculateNewPositionWithAim (s: Result) (currentItem: Direction) =
        match currentItem with
        | Down x -> { s with Aim = s.Aim + x }
        | Up x -> { s with Aim = s.Aim - x }
        | Forward x -> {
            s with
                Horizontal = s.Horizontal + x
                Vertical = s.Vertical + (s.Aim * x)
          }

    let getResult folder (data: string seq) =
        data
        |> Seq.map (Line().TypedMatch)
        |> Seq.map (fun m -> Direction.mk m.Direction.Value (m.Distance.Value |> int))
        |> Seq.fold folder Result.blank
        |> Result.calcOutput
