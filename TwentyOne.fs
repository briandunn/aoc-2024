module TwentyOne

type Pt = int * int

let maxInt = System.Int32.MaxValue

type Direction =
    | U
    | D
    | L
    | R
    | A

let print moves =
    moves
    |> Seq.map (function
        | U -> "^"
        | D -> "v"
        | L -> "<"
        | R -> ">"
        | A -> "A")
    |> String.concat ""
    |> printfn "%s"

let move d (x, y) =
    match d with
    | U -> x, y - 1
    | D -> x, y + 1
    | L -> x - 1, y
    | R -> x + 1, y
    | A -> x, y

let neighbors pt =
    List.map (fun d -> d, move d pt) [ R; D; U; L ]

let intersperse sep =
    Seq.fold (fun acc x -> x :: sep :: acc) [] >> Seq.ofList

module NumPad =
    type Key =
        | Zero
        | One
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | A

    let start = 2, 3

    let moveBetween (valid: Pt Set) ((x, y): Pt as finish) ((x', y'): Pt) : Pt * Direction seq =
        let vertical = List.replicate (abs (y - y')) (if y < y' then U else D)
        let horizontal = List.replicate (abs (x - x')) (if x < x' then L else R)

        let toPoints =
            let fold (pt, points) d =
                let pt = move d pt
                pt, pt :: points

            List.fold fold ((x', y'), []) >> snd >> Set.ofList

        let isValid = toPoints >> Set.isSuperset valid

        let sortBy =
            function
            | L :: _ -> 0
            | D :: _ -> 1
            | U :: _ -> 2
            | _ -> 3

        (finish,
         [ vertical @ horizontal; horizontal @ vertical ]
         |> List.sortBy sortBy
         |> List.find isValid
         |> Seq.ofList)

    let valid =
        seq {
            for x in 0..2 do
                for y in 0..3 -> x, y
        }
        |> Set.ofSeq
        |> Set.remove (0, 3)

    let tryButton pt =
        match pt with
        | 1, 3 -> Some Zero
        | 0, 2 -> Some One
        | 1, 2 -> Some Two
        | 2, 2 -> Some Three
        | 0, 1 -> Some Four
        | 1, 1 -> Some Five
        | 2, 1 -> Some Six
        | 0, 0 -> Some Seven
        | 1, 0 -> Some Eight
        | 2, 0 -> Some Nine
        | 2, 3 -> Some A
        | _ -> None

    let moveTo =
        function
        | Zero -> 1, 3
        | One -> 0, 2
        | Two -> 1, 2
        | Three -> 2, 2
        | Four -> 0, 1
        | Five -> 1, 1
        | Six -> 2, 1
        | Seven -> 0, 0
        | Eight -> 1, 0
        | Nine -> 2, 0
        | A -> 2, 3
        >> moveBetween valid

    let expand: Key seq -> Direction seq =
        let fold (position, moves) key =
            let dest, moves' = moveTo key position
            dest, (List.ofSeq moves') :: moves

        Seq.fold fold (start, []) >> snd >> intersperse [ Direction.A ] >> Seq.concat

module DPad =
    let start = A

    let moveTo (dest: Direction) (start: Direction) : Direction seq =
        match start, dest with
        | A, D -> [ L; D ]
        | A, L -> [ D; L; L ]
        | A, R -> [ D ]
        | A, U -> [ L ]

        | D, A -> [ U; R ]
        | D, L -> [ L ]
        | D, R -> [ R ]
        | D, U -> [ U ]

        | L, A -> [ R; R; U ]
        | L, D -> [ R ]
        | L, R -> [ R; R ]
        | L, U -> [ R; U ]

        | R, A -> [ U ]
        | R, D -> [ L ]
        | R, L -> [ L; L ]
        | R, U -> [ L; U ]

        | U, A -> [ R ]
        | U, D -> [ D ]
        | U, L -> [ D; L ]
        | U, R -> [ D; R ]
        | _ -> []

let parse: string seq -> NumPad.Key list list =
    let map line =
        seq {
            for c in line do
                match c with
                | '0' -> yield NumPad.Zero
                | '1' -> yield NumPad.One
                | '2' -> yield NumPad.Two
                | '3' -> yield NumPad.Three
                | '4' -> yield NumPad.Four
                | '5' -> yield NumPad.Five
                | '6' -> yield NumPad.Six
                | '7' -> yield NumPad.Seven
                | '8' -> yield NumPad.Eight
                | '9' -> yield NumPad.Nine
                | 'A' -> yield NumPad.A
                | _ -> ()
        }

    Seq.map (map >> Seq.toList) >> Seq.toList

// would only contain 16 keys. each generation roughly doubles, wer're at 146689854 for 7 so 18B for 1...
// pair -> level -> seq
type Cache = Map<Direction * Direction, Map<int, Direction seq>>
// what if we unroll each of the 16 25 times?
module Cache =
    let update
        ((src, dest) as pair: Direction * Direction)
        (level: int)
        (fn: Direction -> Direction -> Direction seq)
        (cache: Cache)
        : Cache * Direction seq =

        let add m =
            let value = fn src dest
            (Map.add pair (Map.add level value m) cache, value)

        match Map.tryFind pair cache with
        | None -> add Map.empty
        | Some cache' ->
            match Map.tryFind level cache' with
            | Some hit -> (cache, hit)
            | None -> add cache'

// let moves' dpadCount (code: NumPad.Key seq) =
//     printfn "%A" code

//     let expand (code: Direction seq) : Direction seq =
//         let fold (position, moves) key =
//             let dest, moves' = DPad.moveTo key position
//             dest, (List.ofSeq moves') :: moves

//         code |> Seq.fold fold (DPad.start, []) |> snd |> intersperse [ A ] |> Seq.concat

//     let rec loop cache n (moves: Direction seq) =
//         printfn "n: %d\tlength: %d" n (Seq.length moves)

//         match n with
//         | n when n = dpadCount -> moves
//         | n -> loop cache (n + 1) (expand moves)

//     loop Map.empty 0 (NumPad.expand code) |> Seq.length

let moves dpadCount (code: NumPad.Key seq) =
    printfn "%A" code

    let fold n (start, cache, moves) dest =
        print moves

        let cache, moves' = cache |> Cache.update (start,dest) n DPad.moveTo

        dest, cache, Seq.concat [ moves; moves' ]

    let rec loop n (start, cache, moves) =
        if n = dpadCount then
            Seq.length moves
        else
            loop (n + 1) (fold n (start, cache, Seq.empty) dest)


    code
    |> NumPad.expand
    |> Seq.fold (fold 0) (DPad.start, Map.empty, Seq.empty)
    |> loop 1
    |> fun x ->
        printfn "length: %d" x
        x

let numericPart =
    let fold (place, acc) =
        function
        | NumPad.One -> 1, 1
        | NumPad.Two -> 1, 2
        | NumPad.Three -> 1, 3
        | NumPad.Four -> 1, 4
        | NumPad.Five -> 1, 5
        | NumPad.Six -> 1, 6
        | NumPad.Seven -> 1, 7
        | NumPad.Eight -> 1, 8
        | NumPad.Nine -> 1, 9
        | NumPad.Zero -> 1, 0
        | _ -> 0, 0
        >> fun (shift, n) -> (place + shift, acc + n * pown 10 place)

    List.rev >> List.fold fold (0, 0) >> snd

let complexity dpadCount code =
    (moves dpadCount code) * (numericPart code)

let one: string seq -> int =
    parse >> List.take 1 >> List.map (complexity 2) >> List.sum

let two: string seq -> int = parse >> List.map (complexity 25) >> List.sum

// let two' lines =
//     let parse line =
//         seq {
//             for c in line do
//                 match c with
//                 | '^' -> yield U
//                 | 'v' -> yield D
//                 | '<' -> yield L
//                 | '>' -> yield R
//                 | 'A' -> yield A
//                 | _ -> ()
//         }

//     let execute start tryButton =
//         let fold (pt, out) =
//             function
//             | A -> pt, pt |> tryButton |> Option.map (fun b -> b :: out) |> Option.defaultValue out
//             | button -> (move button pt), out

//         Seq.fold fold (start, []) >> snd >> List.rev

//     lines
//     |> Seq.tryExactlyOne
//     |> Option.map (
//         parse
//         >> execute DPad.start DPad.tryButton
//         >> fun x ->
//             print x
//             x
//         >> execute DPad.start DPad.tryButton
//         >> fun x ->
//             print x
//             x
//         >> execute NumPad.start NumPad.tryButton
//         >> printfn "%A"
//     )
//     |> ignore

//     0
