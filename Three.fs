module Three

let one: string seq -> int =
    let parse line =
        let map (m: System.Text.RegularExpressions.Match) =
            String.parseInt m.Groups.[1].Value, String.parseInt m.Groups.[2].Value

        System.Text.RegularExpressions.Regex("mul\((\d+),(\d+)\)").Matches(line)
        |> Seq.map map

    Seq.map (parse >> Seq.map (fun (x, y) -> x * y) >> Seq.sum) >> Seq.sum

type Instruction =
    | Mul of int * int
    | Do
    | Don't

let two: string seq -> int =
    let parse line =
        let map (m: System.Text.RegularExpressions.Match) =
            [ "mul"; "do"; "dont" ]
            |> List.tryFind (fun name -> m.Groups.Item(name).Success)
            |> function
                | Some "mul" ->
                    (String.parseInt m.Groups.[1].Value, String.parseInt m.Groups.[2].Value)
                    |> Mul
                    |> Some
                | Some "do" -> Some Do
                | Some "dont" -> Some Don't
                | _ -> None

        System.Text.RegularExpressions
            .Regex("(?<mul>mul\((\d+),(\d+)\))|(?<do>do\(\))|(?<dont>don't\(\))")
            .Matches(line)
        |> Seq.choose map

    let foldAll =
        let foldLine (enabled, sum) =
            function
            | Mul(x, y) when enabled -> (true, sum + x * y)
            | Mul _ -> (false, sum)
            | Do -> (true, sum)
            | Don't -> (false, sum)

        Seq.fold foldLine

    Seq.map parse
    >> Seq.fold foldAll (true, 0)
    >> function
        | (_, sum) -> sum
