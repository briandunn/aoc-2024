module Seven

let parse: string seq -> (int64 * (int64 seq)) seq =
    let choose line =
        line
        |> String.split ": "
        |> Seq.toList
        |> function
            | [ test; inputs ] ->
                (int64 test, inputs |> String.split "\s+" |> Seq.map int64)
                |> Some
            | _ -> None

    Seq.choose choose

let one lines =
    let rec operatorCombinations inputs =
        match inputs with
        | a :: b :: rest ->
            [ a + b; a * b ]
            |> List.map (fun x -> operatorCombinations (x :: rest))
            |> List.concat
        | a :: [] -> [ a ]
        | [] -> []

    let filter (test, inputs) =
        inputs
        |> Seq.toList
        |> operatorCombinations
        |> Seq.contains test

    lines
    |> (parse
        >> Seq.filter filter
        >> Seq.map (fun (test, _) -> test)
        >> Seq.sum)
    |> printfn "%d"

    0

let two lines =
    let rec operatorCombinations inputs =
        match inputs with
        | a :: b :: rest ->
            [ a + b
              a * b
              b |> sprintf "%d%d" a |> int64 ]
            |> List.map (fun x -> operatorCombinations (x :: rest))
            |> List.concat
        | a :: [] -> [ a ]
        | [] -> []

    let filter (test, inputs) =
        inputs
        |> Seq.toList
        |> operatorCombinations
        |> Seq.contains test

    lines
    |> (parse
        >> Seq.filter filter
        >> Seq.map (fun (test, _) -> test)
        >> Seq.sum)
    |> printfn "%d"

    0
