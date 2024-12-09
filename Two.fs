module Two

let parse = Seq.map (String.split "\s+" >> (Seq.map String.parseInt))

let inRange i = List.contains i [ 1; 2; 3 ]

let one: string seq -> int =
    let rec isSafe =
        function
        | (first :: second :: _) as levels -> isSafe' (first < second) levels
        | _ -> true // one level

    and isSafe' ascending =
        function
        | first :: second :: rest when ascending && inRange (second - first) -> isSafe' ascending (second :: rest)
        | first :: second :: rest when not ascending && inRange (first - second) -> isSafe' ascending (second :: rest)
        | [ _ ] -> true
        | _ -> false

    parse
    >> Seq.filter (Seq.toList >> isSafe)
    >> Seq.length

type Dampened =
    | First
    | Second

let two: string seq -> int =
    let rec isSafe dampened =
        function
        | (first :: second :: _) as levels ->
            match splitSafe (first < second) [] levels with
            | _, [] -> true
            | _ :: safe, unchecked when not (Set.contains First dampened) ->
                isSafe (Set.add First dampened) ((List.rev safe) @ unchecked)
            | safe, _ :: unchecked when not (Set.contains Second dampened) ->
                isSafe (Set.add Second dampened) ((List.rev safe) @ unchecked)
            | _ -> false
        | _ -> true // one level

    and splitSafe ascending safe =
        function
        | first :: second :: rest when ascending && inRange (second - first) ->
            splitSafe ascending (first :: safe) (second :: rest)
        | first :: second :: rest when not ascending && inRange (first - second) ->
            splitSafe ascending (first :: safe) (second :: rest)
        | [ last ] -> last :: safe, []
        | rest -> safe, rest

    parse
    >> Seq.filter (Seq.toList >> (isSafe Set.empty))
    >> Seq.length
