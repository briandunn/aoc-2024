module TwentyFive

type Schematic =
    | Key
    | Lock

let parse lines  =
    let fold (locks, keys) (lines: string list) =
        let change y =
            function
            | Some(yMin, yMax) -> min y yMin, max y yMax
            | None -> y, y
            >> Some

        let fold map (x, y) = Map.change x (change y) map

        let toSchematic map =
            let values = Map.values map

            if Seq.forall (fst >> ((=) 0)) values then
                (Set.add (values |> Seq.map snd |> Seq.toArray) locks, keys)
            else
                (locks, Set.add (values |> Seq.map fst |> Seq.toArray) keys)

        seq {
            for y, line in lines |> Seq.indexed do
                for x, c in Seq.indexed line do
                    if c = '#' then
                        yield x, y
        }
        |> Seq.fold fold Map.empty
        |> toSchematic


    let rec loop (schematic: string list) acc lines =
        match Seq.tryHead lines with
        | None -> schematic |> List.rev |> fold acc
        | Some "" -> loop [] (fold acc (List.rev schematic)) (Seq.tail lines)
        | Some line -> loop (line::schematic) acc (Seq.tail lines)

    lines |> Seq.cache |> loop [] (Set.empty, Set.empty)

let one (lines: string seq) =
    lines |> parse |> printfn "%A"
    0

let two _ = 0
