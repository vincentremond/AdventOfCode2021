// module Option =
//     let ofTuple t =
//         match t with
//         | Some a, Some b -> Some(a, b)
//         | _ -> None

// let add a b = fst a + fst b, snd a + snd b

// let mapT f a = (f (fst a), f (snd a))

// let mapT3 f a b c =
//     (f (fst a) (fst b) (fst c), f (snd a) (snd b) (snd c))

// let inc v i m = v + i
// // let r = v + i
// // if r > m then None else Some r

// let forTupple s e i =
//     let generator (state: int * int) : Option<(int * int) * (int * int)> =
//         let x = mapT3 inc s i e


//         Some(x, state)

//     Seq.unfold generator s

// printf
//     "%A"
//     (forTupple (0, 0) (5, 5) (1, 1)
//      |> Seq.take 100
//      |> Seq.toList)


// let max = 6

// let generator state =
//     if state > max then
//         None
//     else
//         Some(state, state + 2)

// List.unfold generator 1


let start = (0, 5)
let max = (5, 0)
let inc = (1, -1)


module Tuple =
    let map2 f a b =
        (f (fst a) (fst b), f (snd a) (snd b))


let generator2 state =
    let stateA, stateB = state
    match Tuple.map2 (>) state max with
    | true, true -> None
    | false, false -> 
        let next = Tuple.map2 (+) state inc
        Some(state, next)
    | _ -> failwith "Invalid operation"

List.unfold generator2 start

