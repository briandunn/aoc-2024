module Twenty

type Pt = int * int

type Track = Pt list

let neighbors dist (x, y) =
    Set.ofList [ x, y - dist; x - dist, y; x + dist, y; x, y + dist ]

let parse lines =
    let neighbors = neighbors 1

    let buildTrack pts start stop =
        let rec loop pts =
            function
            | (head :: _) as track ->
                match neighbors head |> Set.intersect pts |> Seq.tryExactlyOne with
                | Some next -> loop (Set.remove next pts) (next :: track)
                | None when head = stop -> List.rev track
                | None -> []
            | _ -> []

        loop pts [ start ]

    seq {
        for y, line in Seq.indexed lines do
            for x, c in Seq.indexed line do
                if c = '.' || c = 'S' || c = 'E' then
                    yield x, y, c
    }
    |> Seq.fold
        (fun ((start, stop, track) as acc) ->
            function
            | x, y, '.' -> (start, stop, Set.add (x, y) track)
            | x, y, 'E' -> (start, Some(x, y), track)
            | x, y, 'S' -> (Some(x, y), stop, track)
            | _ -> acc)
        (None, None, Set.empty)
    |> function
        | Some start, Some stop, track -> buildTrack (Set.add stop track) start stop
        | _ -> []

let one lines =
    let gap = 100
    let track = lines |> parse |> Array.ofList

    let rec loop count i =
        let rest = i + gap

        if Array.length track - rest < 1 then
            count
        else
            let pt = Array.item i track
            let withinJump = neighbors 2 pt

            let fold (i, count) pt =
                if i > rest && Set.contains pt withinJump then
                    i + 1, count + 1
                else
                    i + 1, count

            loop (track |> Array.fold fold (0, count) |> snd) (i + 1)

    loop 0 0

let two lines = 0
