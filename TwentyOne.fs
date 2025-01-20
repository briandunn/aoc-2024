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
    let moveTo start dest =
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

// stole from https://github.com/tmo1/adventofcode/blob/main/2024/21b.py#L27C1-L38C13
let rec nextRobot cache sequence level =
    let toAcc cache len =
        Map.add (sequence, level) len cache, len

    match Map.tryFind (sequence, level) cache with
    | Some len -> cache, len
    | None when level = 0 -> sequence |> Seq.length |> int64 |> toAcc cache
    | None ->
        let fold ((cache, len) as acc) =
            function
            | [| a; b |] ->
                let cache, len' = nextRobot cache (DPad.moveTo a b @ [ A ]) (level - 1)
                toAcc cache (len + len')
            | _ -> acc

        sequence |> Seq.append [ A ] |> Seq.windowed 2 |> Seq.fold fold (cache, 0L)

let moves dpadCount code =
    dpadCount
    |> nextRobot Map.empty (code |> NumPad.expand |> List.ofSeq)
    |> snd

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
    (moves dpadCount code) * (code |> numericPart |> int64)

let one: string seq -> int =
    parse
    >> List.map (complexity 2)
    >> List.sum
    >> (fun x ->
        printfn "%d" x
        0)

let two: string seq -> int =
    parse
    >> List.map (complexity 25)
    >> List.sum
    >> (fun x ->
        printfn "%d" x
        0)
