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

    let setInt c i state =
        let fold state (b, v) =
            Map.add (sprintf "%s%02d" c b) (int v) state

        seq { for b in 0..63 -> b, i >>> b &&& 1L } |> Seq.fold fold state

let one lines =
    let { state = state; gates = gates } = parse lines

    state |> Device.run gates |> Device.toInt "z" |> printfn "%d"
    0

let toDot: Gate seq -> unit =
    Seq.sortBy (fun { output = o } -> o)
    >> Seq.map (fun { output = o; inputs = (a, b); op = op } ->
        [ a; b ]
        |> Seq.map (sprintf "%s -> %s;")
        |> Seq.map ((|>) o)
        |> Seq.append [ sprintf "%s [label=\"%A\"];" o op ]
        |> String.concat "\n")
    >> String.concat "\n"
    >> printfn
        "digraph {
      %s
    }"

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
        (Device.run gates state |> Device.toInt "z")

    let partition b =
        let input = 1L <<< b
        test input = input * 2L

    let pass, fail = [ 0..maxBit ] |> List.partition partition

    0
