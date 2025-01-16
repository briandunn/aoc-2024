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

let moveBetween (valid: Pt Set) (finish: Pt) (start: Pt) : Pt * (Direction list) =
    let neighbors = neighbors >> List.filter (fun (_, pt) -> Set.contains pt valid)

    let rec loop shortest completed scores pq =
        match PQ.tryPop pq with
        | None -> List.map List.rev completed
        | Some(_, (pt, _)) when pt = finish && (Map.find pt scores) > shortest -> completed
        | Some(pq, (pt, path)) when pt = finish -> loop (Map.find pt scores) (path :: completed) scores pq
        | Some(pq, (pt, path)) ->
            let score = Map.find pt scores

            let fold (pq, scores) (direction, neighbor) =
                let score =
                    path
                    |> List.tryHead
                    |> function
                        | Some d when d = direction -> score + 1
                        | _ -> score + 10

                scores
                |> Map.tryFind neighbor
                |> function
                    | Some prevScore when prevScore < score -> (pq, scores)
                    | _ -> PQ.push score (neighbor, direction :: path) pq, Map.add neighbor score scores

            let pq, scores = pt |> neighbors |> List.fold fold (pq, scores)
            loop shortest completed scores pq

    let shortestPaths =
        loop maxInt [] (Map.add start 0 Map.empty) (PQ.push 0 (start, []) Map.empty)

    // if List.length shortestPaths > 1 then
    //     printfn "Shortest paths from %A to %A" start finish
    //     List.iter print shortestPaths

    (finish, List.head shortestPaths)

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

    let moveTo =
        function
        | U -> 1, 0
        | D -> 1, 1
        | L -> 0, 1
        | R -> 2, 1
        | A -> 2, 0
        >> moveBetween valid


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
    let expand move start =
        let fold (position, moves) key =
            let dest, moves' = move key position
            dest, moves @ moves' @ [ A ]

        List.fold fold (start, [])
        >> snd
        >> fun x ->
            print x
            x

    code
    |> expand NumPad.moveTo NumPad.start
    |> expand DPad.moveTo DPad.start
    |> expand DPad.moveTo DPad.start

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

    0

// <v<A>>^AvA^A< vA<AA>>^AAvA<^A>A
// <<vA^>>AvA^A< <vA^>>A<<vA>A>^AA<A>vA^
