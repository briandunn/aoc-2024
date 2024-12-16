module Five

type Input =
    { rules: (int * int) list
      updates: int list list }

type Section =
    | Rules
    | Updates

let parse: string seq -> Input =
    let fold (section, input) line =
        match section with
        | Rules ->
            match line |> String.split "\|" |> Seq.toList with
            | [ a; b ] ->
                section,
                { input with
                    rules = (int a, int b) :: input.rules }
            | l -> (Updates, input)
        | Updates ->
            (section,
             { input with
                 updates = (line |> String.split "," |> Seq.map int |> Seq.toList) :: input.updates })

    Seq.fold fold (Rules, { rules = []; updates = [] })
    >> function
        | (_, input) ->
            { input with
                updates = List.rev input.updates }

let middle update =
    List.item ((List.length update) / 2) update

let toRuleMap rules =
    let fold acc (a, b) =
        let change =
            function
            | Some values -> Set.add b values
            | None -> Set.singleton b
            >> Some

        Map.change a change acc

    List.fold fold Map.empty rules

let getViolators a rest rules =
    match Map.tryFind a rules with
    | Some b -> Set.difference (Set.ofList rest) b
    | None when rest = [] -> Set.empty
    | None -> Set.singleton a

let inOrder a rest = getViolators a rest >> Set.isEmpty

let one lines =
    let input = parse lines

    let rules = toRuleMap input.rules

    let rec isCorrect =
        function
        | a :: rest when inOrder a rest rules -> isCorrect rest
        | [] -> true
        | _ -> false

    input.updates |> List.filter isCorrect |> List.map middle |> List.sum

let two lines =
    let input = parse lines

    let rules = toRuleMap input.rules

    let sortWith a b =
        match Map.tryFind a rules with
        | Some afters when Set.contains b afters -> -1
        | Some _
        | None ->
            match Map.tryFind b rules with
            | Some afters when Set.contains a afters -> 1
            | _ -> 0

    let correct update =
        let corrected = List.sortWith sortWith update
        if update = corrected then None else Some corrected

    let fold corrected =
        correct
        >> function
            | Some update -> update :: corrected
            | None -> corrected

    input.updates |> List.fold fold [] |> List.map middle |> List.sum
