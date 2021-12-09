module AdventOfCode2021.Day01.Tests.Part1

open AdventOfCode2021.Common
open NUnit.Framework
open Swensen.Unquote

[<Test>]
let ``Test List.permutations`` () =
    test
        <@ Tuple.apply
            List.sort
            (List.permutations [ 1; 2; 3 ],
             [
                 [ 1; 2; 3 ]
                 [ 1; 3; 2 ]
                 [ 2; 1; 3 ]
                 [ 2; 3; 1 ]
                 [ 3; 1; 2 ]
                 [ 3; 2; 1 ]
             ])
           |> Tuple.fold (=) @>

[<Test>]
let testListRange () = test <@ List.range 0 3 = [ 0; 1; 2 ] @>

[<Test>]
let Test_Seq_range_0_10 () =
    test <@ (Seq.range 0 10 |> Seq.toArray) = ({ 0 .. 9 } |> Seq.toArray) @>
