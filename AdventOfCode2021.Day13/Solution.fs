namespace AdventOfCode2021.Day13

open System.Drawing
open System.IO
open AdventOfCode2021.Common
open Tesseract

type X = int
type Y = int

type Point = Y * X

type FoldingLine =
    | Horizontal of Y
    | Vertical of X

module Solution =
    let parseInput lines =
        let rec parseInput' lines points folds =
            match lines with
            | line :: others ->
                match line with
                | "" -> parseInput' others points folds
                | IsMatch @"^(?<X>\d+),(?<Y>\d+)$" m ->
                    let point: Point =
                        (m.Groups.["Y"].Value |> int, m.Groups.["X"].Value |> int)

                    parseInput' others (point :: points) folds
                | IsMatch @"^fold along (?<Axis>\w)=(?<Distance>\d+)$" m ->
                    let distance = m.Groups.["Distance"].Value |> int

                    let fold =
                        let axis = m.Groups.["Axis"].Value

                        match axis with
                        | "x" -> Vertical distance
                        | "y" -> Horizontal distance
                        | _ -> failwith $"Parse error, invalid axis '{axis}'"

                    parseInput' others points (fold :: folds)
                | _ -> failwith $"Parse error, invalid axis '{line}'"
            | [] -> (points, folds |> List.rev)

        parseInput' lines [] []

    let convPoint foldA a =
        match (a - foldA) with
        | 0 -> None
        | a' ->
            let i = foldA - (a' |> abs)

            if i >= 0 then
                Some(i)
            else
                failwith "Humm 🤔"

    let mapTuple (f1, f2) (a1, a2) = (f1 a1, f2 a2)

    let foldPoint fold =
        (match fold with
         | Vertical x -> mapTuple (Some, (convPoint x))
         | Horizontal y -> mapTuple ((convPoint y), Some))
        >> Option.unfold

    let displayChars (points: Point seq) : string =

        let maxY, maxX =
            Seq.fold (fun state item -> Tuple.map2 max item state) (0, 0) points

        let bmp = new Bitmap(maxX + 3, maxY + 3)
        use graph = Graphics.FromImage(bmp)

        let r =
            new Rectangle(0, 0, bmp.Width, bmp.Height)

        graph.FillRectangle(Brushes.White, r)

        for y, x in points do
            bmp.SetPixel(x + 1, y + 1, Color.Black)

        let bmp =
            new Bitmap(bmp, new Size(bmp.Width * 8, bmp.Height * 8))

        let ms = new MemoryStream()
        bmp.Save(ms, Imaging.ImageFormat.Bmp)
        ms.Flush()
        ms.Position <- 0
        let barr = ms.ToArray()

        File.WriteAllBytes(@".\data\test.bmp", barr)

        let eng =
            new TesseractEngine(@".\data\", "eng", EngineMode.TesseractOnly)

        use img = Pix.LoadFromMemory(barr)
        use page = eng.Process(img)
        page.GetLSTMBoxText(0)

    let calc (foldFilter: FoldingLine list -> FoldingLine list) (calculateResult: Point list -> 'a) inputs =

        let points, folds = inputs |> Seq.toList |> parseInput
        let folds = folds |> foldFilter

        let folder (points: Point list) (paperFold: FoldingLine) : Point list =
            points
            |> Seq.choose (foldPoint paperFold)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toList

        let finalPoints = List.fold folder points folds

        finalPoints |> calculateResult

    let part1: string seq -> int = calc (List.take 1) (List.length)
    let part2: string seq -> string = calc id displayChars
