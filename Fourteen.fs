module Fourteen

type Robot =
    { position: int * int
      velocity: int * int }

let regex = System.Text.RegularExpressions.Regex("-?\d+")

let parse =
    let parse line =
        match regex.Matches(line) |> Seq.map (fun m -> int m.Value) |> Seq.toArray with
        | [| px; py; vx; vy |] ->
            Some(
                { position = (px, py)
                  velocity = (vx, vy) }
            )
        | _ -> None

    Seq.choose parse >> Seq.cache

// let dimensions = (11, 7)
let dimensions = (101, 103)

let (w, h) = dimensions
let mx = w / 2
let my = h / 2

let advance
    ({ position = (px, py)
       velocity = (vx, vy) } as robot)
    =
    let x = (px + vx) % w
    let x = if x < 0 then x + w else x
    let y = (py + vy) % h
    let y = if y < 0 then y + h else y
    { robot with position = x, y }

let print (w, h) robots =
    let robotMap = robots |> Seq.groupBy (fun r -> r.position) |> Map.ofSeq

    // printf "%c[H" (char 0x1B)

    seq {
        for y in 0 .. (h - 1) do
            for x in 0 .. (w - 1) do
                yield
                    match Map.tryFind (x, y) robotMap with
                    | Some r -> r |> Seq.length |> string
                    | None -> "."

            yield "\n"
    }
    |> Seq.iter (printf "%s")

    robots

module One =
    let quadrant =
        function
        | (x, y) when x < mx && y < my -> Some 0
        | (x, y) when x > mx && y < my -> Some 1
        | (x, y) when x < mx && y > my -> Some 2
        | (x, y) when x > mx && y > my -> Some 3
        | _ -> None

    let one robots =
        let fold robots _ = Seq.map advance robots

        seq { 1..100 }
        |> Seq.fold fold (print dimensions robots)
        |> print dimensions
        |> Seq.countBy (fun robot -> quadrant robot.position)
        |> Seq.fold
            (fun product ->
                function
                | Some _, count -> count * product
                | None, _ -> product)
            1

module Two =
    let groupNeighbors: Robot seq -> ((int * int) Set) list =
        let rec loop groups =
            function
            | [] -> groups
            | (px, py) as bot :: rest ->
                let neighbors =
                    seq {
                        for y in py - 1 .. py + 1 do
                            for x in px - 1 .. px + 1 do
                                yield (x, y)
                    }
                    |> Set.ofSeq

                let groups =
                    match List.partition (fun group -> neighbors |> Set.intersect group |> Set.isEmpty) groups with
                    | strangers, [] -> (Set.singleton bot) :: strangers
                    | strangers, neighborhoods -> (neighborhoods |> Set.unionMany |> Set.add bot) :: strangers

                loop groups rest

        Seq.map (fun { position = p } -> p) >> Seq.toList >> loop []

    let containsGroupOf count =
        groupNeighbors >> List.exists (fun group -> group |> Set.count >= count)

    let two =
        let rec loop i robots =
            if containsGroupOf 50 robots then
                print dimensions robots |> ignore
                i
            else
                loop (i + 1) (List.map advance robots)

        loop 0

let one: string seq -> int = parse >> One.one
let two: string seq -> int = parse >> Seq.toList >> Two.two
