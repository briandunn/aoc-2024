module Twenty

type Pt = int * int

type Track = Pt array

let neighbors dist (x, y) =
    Set.ofList [ x, y - dist; x - dist, y; x + dist, y; x, y + dist ]

let parse lines =
    let neighbors = neighbors 1

    let buildTrack pts start =
        let rec loop pts =
            function
            | (head :: _) as track ->
                match head |> neighbors |> Set.intersect pts |> Seq.tryExactlyOne with
                | Some next -> loop (Set.remove next pts) (next :: track)
                | None -> List.rev track
            | _ -> []

        loop pts [ start ] |> Array.ofList

    seq {
        for y, line in Seq.indexed lines do
            for x, c in Seq.indexed line -> x, y, c
    }
    |> Seq.fold
        (fun ((start, track) as acc) ->
            function
            | x, y, '.'
            | x, y, 'E' -> (start, Set.add (x, y) track)
            | x, y, 'S' -> (Some(x, y), track)
            | _ -> acc)
        (None, Set.empty)
    |> function
        | Some start, track -> buildTrack track start
        | _ -> Array.empty

let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let countJumps gap maxCheat track =
    let trackLength = Array.length track

    let rec loop count cheatStartIndex =
        if trackLength - cheatStartIndex - gap < 1 then
            count
        else
            let cheatStart = Array.item cheatStartIndex track

            let fold (cheatStopIndex, count) cheatStop =
                let jumpDist = dist cheatStart cheatStop

                if jumpDist <= maxCheat && cheatStopIndex - cheatStartIndex - jumpDist >= gap then
                    cheatStopIndex + 1, count + 1
                else
                    cheatStopIndex + 1, count

            loop (track |> Array.fold fold (0, count) |> snd) (cheatStartIndex + 1)

    loop 0 0

let one: string seq -> int = parse >> countJumps 100 2
let two: string seq -> int = parse >> countJumps 100 20
