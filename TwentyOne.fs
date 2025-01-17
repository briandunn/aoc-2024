module TwentyOne

type Pt = int * int

let maxInt = System.Int32.MaxValue

type Direction =
    | U
    | D
    | L
    | R
    | A

let print =
    List.map (function
        | U -> "^"
        | D -> "v"
        | L -> "<"
        | R -> ">"
        | A -> "A")
    >> String.concat ""
    >> printfn "%s"

let move d (x, y) =
    match d with
    | U -> x, y - 1
    | D -> x, y + 1
    | L -> x - 1, y
    | R -> x + 1, y
    | A -> x, y

let neighbors pt =
    List.map (fun d -> d, move d pt) [ R; D; U; L ]

let moveBetween (valid: Pt Set) ((x, y): Pt as finish) ((x', y'): Pt) : Pt * Direction list list =
    let vertical = List.replicate (abs (y - y')) (if y < y' then U else D)
    let horizontal = List.replicate (abs (x - x')) (if x < x' then L else R)

    let toPoints =
        let fold (pt, points) d =
            let pt = move d pt
            pt, pt :: points

        List.fold fold ((x', y'), []) >> snd >> Set.ofList

    let isValid = toPoints >> Set.isSuperset valid

    // perhaps its the choice where that is the fewest moves from the last choice?
    // and if tied the one closes to the next, and if both tied, who cares.
    let paths = List.distinct [ vertical @ horizontal; horizontal @ vertical ]

    (finish, List.filter isValid paths)


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

module DPad =
    let start = 2, 0

    let valid =
        seq {
            for x in 0..2 do
                for y in 0..1 -> x, y
        }
        |> Set.ofSeq
        |> Set.remove (0, 0)

    let tryButton pt =
        match pt with
        | 1, 0 -> Some U
        | 1, 1 -> Some D
        | 0, 1 -> Some L
        | 2, 1 -> Some R
        | 2, 0 -> Some A
        | _ -> None

    let coords =
        function
        | U -> 1, 0
        | D -> 1, 1
        | L -> 0, 1
        | R -> 2, 1
        | A -> 2, 0

    let moveTo = coords >> moveBetween valid


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

let rec permute' choices =
    let map rest el =
        let map rest' = el :: rest'
        rest |> permute' |> List.map map

    match choices with
    | head :: rest -> head |> List.map (map rest) |> List.concat
    | [] -> [ [] ]

let permute (choices: 'a list list) : 'a list list =
    let rec loop acc choices =
        let map head =
            let map tail = head :: tail
            List.map map acc

        match choices with
        | head :: rest when acc = [] -> loop (List.map List.singleton head) rest
        | head :: rest -> loop (head |> List.map map |> List.concat) rest
        | [] -> List.map List.rev acc

    loop [] choices

let distanceBetweenButtons =
    let dist (x, y) (x', y') = (abs (y - y')) + (abs (x - x'))

    let fold acc (a, b) =
        acc + (dist (DPad.coords a) (DPad.coords b))

    List.pairwise >> List.fold fold 0

let moves (code: NumPad.Key list) =
    let chooseBest =
        permute
        >> List.map List.concat
        >> List.groupBy distanceBetweenButtons
        >> Map.ofList
        >> Map.minKeyValue
        >> snd

    let expand move start moves =
        let fold ((position, moves): Pt * Direction list list) key =
            let dest, moves' = move key position
            dest, (chooseBest ([ moves ] @ [ moves' ] @ [ [ [ A ] ] ]))

        moves |> List.fold fold (start, []) |> snd
    // gotta be a way to choose one as we go

    let rec loop n (moves: Direction list list) =
        // List.iter print moves
        match n with
        | 0 -> moves
        | n ->
            loop (n - 1) (moves |> List.map (expand DPad.moveTo DPad.start) |> List.concat)

    loop 3 (expand NumPad.moveTo NumPad.start code) |> List.minBy List.length

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

let complexity code =
    (code
     |> moves
     |> List.length
     |> fun l ->
         printfn "length:%d" l
         l)
    * (numericPart code)


// 255888 - too high
let one: string seq -> int = parse >> List.map complexity >> List.sum

let two lines =
    let parse line =
        seq {
            for c in line do
                match c with
                | '^' -> yield U
                | 'v' -> yield D
                | '<' -> yield L
                | '>' -> yield R
                | 'A' -> yield A
                | _ -> ()
        }

    let execute start tryButton =
        let fold (pt, out) =
            function
            | A -> pt, pt |> tryButton |> Option.map (fun b -> b :: out) |> Option.defaultValue out
            | button -> (move button pt), out

        Seq.fold fold (start, []) >> snd >> List.rev

    lines
    |> Seq.tryExactlyOne
    |> Option.map (
        parse
        >> execute DPad.start DPad.tryButton
        >> fun x ->
            print x
            x
        >> execute DPad.start DPad.tryButton
        >> fun x ->
            print x
            x
        >> execute NumPad.start NumPad.tryButton
        >> printfn "%A"
    )
    |> ignore

    permute [ [ 1; 2 ]; [ 3 ]; [ 4; 5 ]; [ 6 ] ] |> printfn "%A"

    0
