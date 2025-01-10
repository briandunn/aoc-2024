module Eighteen

type Pt = int * int

let maxInt = System.Int32.MaxValue

let parseLine =
    String.split ","
    >> Seq.map int
    >> Seq.toList
    >> function
        | [ x; y ] -> x, y
        | _ -> failwith "Invalid point"

let w = 70
let h = 70
let finish = (w, h)
let start = (0, 0)

let dijkstra walls =
    let inBounds (x, y) = x >= 0 && x <= w && y >= 0 && y <= h

    let neighbors (x, y) =
        walls
        |> Set.difference (
            [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
            |> List.filter inBounds
            |> Set.ofList
        )

    let rec loop q scores =
        match PQ.tryPop q with
        | None -> None
        | Some(_, (pt, path)) when pt = finish -> Some(pt :: path)
        | Some(q, (pt, path)) ->
            let here = Map.find pt scores
            let cost = here + 1

            let fold (q, scores) neighbor =
                let prev = scores |> Map.tryFind neighbor |> Option.defaultValue maxInt

                if cost < prev then
                    PQ.push cost (neighbor, pt :: path) q, Map.add neighbor cost scores
                else
                    q, scores

            let q, steps = pt |> neighbors |> Set.fold fold (q, scores)
            loop q steps

    loop (PQ.push 0 (start, []) Map.empty) (Map.add start 0 Map.empty)

module One =
    let parse: string seq -> Pt Set =

        let fold walls line = Set.add (parseLine line) walls
        Seq.fold fold Set.empty

    let one = Seq.take 1024 >> parse >> dijkstra

let one: string seq -> int =
    One.one
    >> Option.map (fun path -> List.length path - 1)
    >> Option.defaultValue -1

let two lines =
    let parse: string seq -> Pt array = Seq.map parseLine >> Seq.toArray

    let walls = parse lines

    let rec loop min max =
        let mid = (min + max) / 2

        if min = max then
            min
        else
            match walls |> Array.take mid |> Set.ofArray |> dijkstra with
            | None -> loop min (mid - 1)
            | Some path ->
                let path = Set.ofList path

                let skipped =
                    (walls
                     |> Array.skip mid
                     |> Array.takeWhile (fun pt -> path |> Set.contains pt |> not)
                     |> Array.length)

                if skipped = 0 then mid else loop (skipped + mid) max

    walls |> Array.item (walls |> Array.length |> loop 1024) |> printfn "%A"
    0
