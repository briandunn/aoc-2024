module One

let parse: string seq -> int list * int list =
    let fold ((ls, rs) as acc) line =
        line
        |> String.split "\s+"
        |> Seq.map String.parseInt
        |> Seq.toList
        |> function
            | l :: r :: [] -> l :: ls, r :: rs
            | _ -> acc

    Seq.fold fold ([], [])
    >> function
        | (ls, rs) -> List.rev ls, List.rev rs

let one lines =
    let (ls, rs) = parse lines

    List.map2 (fun l r -> abs (l - r)) (List.sort ls) (List.sort rs) |> List.sum

let two lines =
    let (ls, rs) = parse lines
    let freq = rs |> List.countBy id |> Map.ofList

    ls
    |> List.map (fun i -> freq |> Map.tryFind i |> Option.defaultValue 0 |> (*) i)
    |> List.sum
