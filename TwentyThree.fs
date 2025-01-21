module TwentyThree

let one lines =
    let parse =
        Seq.choose (
            String.split "-"
            >> (fun x ->
                match Seq.tryHead x, Seq.tryLast x with
                | Some a, Some b -> Some(a, b)
                | _ -> None)
        )

    let edges = parse lines |> Seq.cache

    let connected map k v =
        match Map.tryFind k map with
        | Some set -> Set.contains v set
        | _ -> false

    let map =
        let add a b =
            let change = Option.defaultValue Set.empty >> Set.add b >> Some
            Map.change a change

        let fold map ((a, b): string * string) = map |> add a b |> add b a
        edges |> Seq.fold fold Map.empty

    let findTriangles triangles k v =
        // find connected pairs from v
        seq {
            for node in v do
                for node' in Set.remove node v do
                    if connected map node node' then
                        yield Set.ofList [ k; node; node' ]
        } |> Seq.fold (fun acc x -> Set.add x acc) triangles

    let startsWithT = Set.exists (String.chars >> Seq.head >> ((=) 't'))

    map |> Map.fold findTriangles Set.empty |> Set.filter startsWithT |> Set.count

let two lines = 0
