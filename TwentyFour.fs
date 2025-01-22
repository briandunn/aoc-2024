module TwentyFour

type Op =
    | AND
    | OR
    | XOR

type Gate =
    { inputs: string * string
      output: string
      op: Op }

type Device =
    { state: Map<string, int>
      gates: Gate seq }

let parse lines =
    let parseInitial =
        let fold map =
            String.split ": "
            >> Seq.toList
            >> function
                | [ wire; signal ] -> Map.add wire (int signal) map
                | _ -> map

        Seq.fold fold Map.empty

    let parseGates =
        let choose =
            String.split " "
            >> Seq.toList
            >> function
                | [ a; op; b; "->"; c ] ->
                    let toGate op =
                        Some { inputs = (a, b); output = c; op = op }

                    match op with
                    | "AND" -> toGate AND
                    | "OR" -> toGate OR
                    | "XOR" -> toGate XOR
                    | _ -> None
                | _ -> None

        Seq.choose choose

    { state = lines |> Seq.takeWhile ((<>) "") |> parseInitial
      gates = lines |> parseGates |> Seq.cache }

module Gate =
    let run
        { inputs = (a, b)
          output = output
          op = op }
        state
        =
        let map2 =
            match op with
            | AND -> (&&&)
            | OR -> (|||)
            | XOR -> (^^^)

        Option.map2 map2 (Map.tryFind a state) (Map.tryFind b state)
        |> Option.map (fun x -> Map.add output x state)
        |> Option.defaultValue state

module Device =
    let run gates =
        let tick = Seq.foldBack Gate.run gates

        let rec run state =
            let state' = tick state
            if state = state' then state else run state'

        run

    let toInt c =
        let setBit b n = n ||| (1L <<< b)

        let fold acc (k, v) =
            k
            |> String.split c
            |> Seq.toList
            |> function
                | [ ""; i ] when v = 1 -> setBit (int i) acc
                | _ -> acc

        Map.toSeq >> Seq.fold fold 0L

    let toEdge = sprintf "%s%02d"

    let setInt c i state =
        let fold state (b, v) = Map.add (toEdge c b) (int v) state

        seq { for b in 0..63 -> b, i >>> b &&& 1L } |> Seq.fold fold state

let one lines =
    let { state = state; gates = gates } = parse lines

    state |> Device.run gates |> Device.toInt "z" |> printfn "%d"
    0

let toDot: Gate seq -> unit =
    let toLabel n l = sprintf "%s [label=\"%A\"]" n l
    let toOutLabel n l = sprintf "%s [label=\"%s %A\"]" n n l

    let fold (nodes, clusters, connections) { output = o; inputs = (a, b); op = op } =
        let nodes, clusters =
            let fold (nodes, clusters) k =
                let c = Seq.head k

                match Map.tryFind c clusters with
                | Some set when c = 'z' -> nodes, Map.add c (Set.add (toOutLabel k op) set) clusters
                | Some set -> nodes, Map.add c (Set.add k set) clusters
                | None -> Set.add (toLabel k op) nodes, clusters

            Seq.fold fold (nodes, clusters) [ a; b; o ]

        let connections =
            [ a; b ]
            |> Seq.map (sprintf "%s -> %s;")
            |> Seq.map ((|>) o)
            |> String.concat "\n"
            |> sprintf "%s\n%s" connections

        nodes, clusters, connections

    let clusters =
        List.fold (fun m k -> Map.add k Set.empty m) Map.empty [ 'x'; 'y'; 'z' ]

    Seq.fold fold (Set.empty, clusters, "")
    >> fun (nodes, clusters, connections) ->
        let nodes = String.concat "\n" nodes

        let clusters =
            clusters
            |> Map.toSeq
            |> Seq.map (fun (k, v) -> sprintf "subgraph cluster_%c {\nordering=out;\n%s\n}" k (String.concat "\n" v))
            |> Seq.fold (sprintf "%s\n%s") ""

        [ "digraph {"; nodes; clusters; connections; "}" ]
        |> String.concat "\n"
        |> printfn "%s"

let two lines =
    let { gates = gates } = parse lines

    let maxBit =
        gates
        |> Seq.map (fun g -> g.output)
        |> Seq.max
        |> String.split "z"
        |> Seq.last
        |> int

    let test input =
        let state = Map.empty |> Device.setInt "x" input |> Device.setInt "y" input
        Device.run gates state |> Device.toInt "z"

    let partition b =
        let input = 1L <<< b
        let output = test input
        let expected = input * 2L
        if output = expected then
            true
        else
            printfn "bit:%d" b
            printfn "expected:%B" expected
            printfn "  actual:%B" output
            false


    let pass, fail = [ 0..maxBit ] |> List.partition partition

    let rec gatherConnected connected =
        let filter wire { output = o } = o = wire
        let fold acc { inputs = (a, b) } = a :: b :: acc

        function
        | head :: rest ->
            let gates = Seq.filter (filter head) gates
            gatherConnected (Seq.append gates connected) ((Seq.fold fold [] gates) @ rest)
        | [] -> connected

    // pass
    // |> List.take 5
    // |> List.map (fun x -> Device.toEdge "z" x)
    // |> gatherConnected Seq.empty
    // |> Seq.distinct
    // |> toDot

    // traverse the graph from like x0, y0, z0
    // these connections *should* be the same for all successful bits, including the cary.
    // follow the cary into the next bit, and check that the connections between nodes and the node types are the same.

    0
