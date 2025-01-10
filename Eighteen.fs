module Eighteen

type Pt = int * int
type Map = { walls: Pt Set; w: int; h: int }

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

    let map = {walls = lines |> Seq.take 1024 |> parse; w = 70; h = 70 }

    printfn "%A" map
    0

let two lines = 0
