module TwentyThree

type Node = string

type Edge = Node * Node

let parse: string seq -> Edge seq =
    Seq.choose (
        String.split "-"
        >> (fun x ->
            match Seq.tryHead x, Seq.tryLast x with
            | Some a, Some b -> Some(a, b)
            | _ -> None)
    )

let toMap: Edge seq -> Map<Node, Node Set> =
    let add a b =
        let change = Option.defaultValue Set.empty >> Set.add b >> Some
        Map.change a change

    let fold map ((a, b): string * string) = map |> add a b |> add b a
    Seq.fold fold Map.empty

let isConnected map k v =
    match Map.tryFind k map with
    | Some set -> Set.contains v set
    | _ -> false

let one lines =
    let map = lines |> parse |> toMap

    let findTriangles triangles k v =
        seq {
            for node in v do
                for node' in Set.remove node v do
                    if isConnected map node node' then
                        yield Set.ofList [ k; node; node' ]
        }
        |> Seq.fold (fun acc x -> Set.add x acc) triangles

    let startsWithT = Set.exists (String.chars >> Seq.head >> ((=) 't'))

    map |> Map.fold findTriangles Set.empty |> Set.filter startsWithT |> Set.count

let two lines =
    let map = lines |> parse |> toMap

    let isConnected = isConnected map

    let rec clump (clumps: Node Set Set) (set: Node Set) : Node Set Set =
        if Set.isEmpty set then
            clumps
        else
            let node = Set.minElement set

            clump
                (Set.map
                    (fun clump ->
                        if Set.forall (isConnected node) clump then
                            Set.add node clump
                        else
                            clump)
                    clumps)
                (Set.remove node set)

    let findClumps clumps all =
        (all |> clump (Set.map Set.singleton all) |> Set.toList) @ clumps

    map
    |> Map.fold (fun acc k v -> Set.add (Set.add k v) acc) Set.empty
    |> Set.fold findClumps []
    |> List.maxBy Set.count
    |> String.concat ","
    |> printfn "%s"

    0
