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

let one lines =
    let { state = state; gates = gates } = parse lines

    let execute
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

    let tick = Seq.foldBack execute gates

    let rec run state =
        let state' = tick state
        if state = state' then state else run state'

    let toInt =
        let setBit b n =
          printfn "%d" b
          n ||| (1L <<< b)

        let fold acc (k, v) =
            k
            |> String.split "z"
            |> Seq.toList
            |> function
                | [ ""; i ] when v = 1 -> setBit (int i) acc
                | _ -> acc

        Map.toSeq >> Seq.fold fold 0L

      // 262651903 too low
    run state |> toInt |> printfn "%d"
    0

let two lines =
  0
