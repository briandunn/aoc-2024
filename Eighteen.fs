module Eighteen

type Pt = int * int
type Map = { walls: Pt Set; w: int; h: int }

let maxInt = System.Int32.MaxValue

let one lines =
    let parse: string seq -> Pt Set =
        let parseLine =
            String.split ","
            >> Seq.map int
            >> Seq.toList
            >> function
                | [ x; y ] -> x, y
                | _ -> failwith "Invalid point"

        let fold walls line = Set.add (parseLine line) walls
        Seq.fold fold Set.empty

    let map =
        { walls = lines |> Seq.take 1024 |> parse
          w = 70
          h = 70 }

    let finish = (map.w, map.h)
    let start = (0, 0)

    let inBounds (x, y) =
        x >= 0 && x <= map.w && y >= 0 && y <= map.h

    let neighbors (x, y) =
        map.walls
        |> Set.difference (
            [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
            |> List.filter inBounds
            |> Set.ofList
        )

    let rec loop q scores =
        match PQ.tryPop q with
        | None ->
          Map.tryFind finish scores
        | Some(_, pt) when pt = finish -> Map.tryFind finish scores
        | Some(q, pt) ->
            let here = Map.find pt scores
            let cost = here + 1

            let fold (q, scores) neighbor =
                let prev = scores |> Map.tryFind neighbor |> Option.defaultValue maxInt

                if cost < prev then
                    PQ.push cost neighbor q, Map.add neighbor cost scores
                else
                    q, scores

            let q, steps = pt |> neighbors |> Set.fold fold (q, scores)
            loop q steps

    loop (PQ.push 0 start Map.empty) (Map.add start 0 Map.empty) |> printfn "%A"
    0

let two lines = 0
