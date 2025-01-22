module TwentyFive

let parse lines =
    let fold (locks, keys) lines =
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


    let rec loop schematic acc lines =
        match Seq.tryHead lines with
        | None -> schematic |> List.rev |> fold acc
        | Some "" -> loop [] (schematic |> List.rev |> fold acc) lines
        | Some line -> loop (line :: schematic) acc lines

    lines |> loop [] (Set.empty, Set.empty)

let one (lines: string seq) =
    let fit lock key =
        Array.zip lock key |> Array.forall (fun (l, k) -> l < k)

    let locks, keys = parse lines

    let fold count lock =
        let fold count key =
            if fit lock key then count + 1 else count

        Set.fold fold count keys

    Set.fold fold 0 locks

let two _ = 0
