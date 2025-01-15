module TwentyOne

type Pt = int * int

type Direction =
    | U
    | D
    | L
    | R
    | A

let moveBetween ((x', y') as dest) (x, y) =
    dest,
    List.replicate (abs (x - x')) (if x < x' then R else L)
    @ List.replicate (abs (y - y')) (if y < y' then D else U)

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

    let start = 2,3

    let moveTo =
        function
        | Zero -> (1, 3)
        | One -> (0, 2)
        | Two -> (1, 2)
        | Three -> (2, 2)
        | Four -> (0, 1)
        | Five -> (1, 1)
        | Six -> (2, 1)
        | Seven -> (0, 0)
        | Eight -> (1, 0)
        | Nine -> (2, 0)
        | A -> (2, 3)
        >> moveBetween

module DPad =
    let start = 2,0

    let moveTo =
        function
        | U -> (1, 0)
        | D -> (1, 1)
        | L -> (0, 1)
        | R -> (2, 1)
        | A -> (2, 0)
        >> moveBetween


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

let moves code =
    let fold (position, moves) key =
        let dest, moves' = NumPad.moveTo key position
        dest, moves @ moves' @ [A]
    code |> List.fold fold (NumPad.start, []) |> snd |> fun x -> printfn "%A" x; x

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

    List.rev >> List.fold fold (0,0) >> snd

let complexity code = (code |> moves |> List.length) * (numericPart code)

let one : string seq -> int = parse >> List.take 1 >> List.map complexity >> List.sum

let two lines = 0
