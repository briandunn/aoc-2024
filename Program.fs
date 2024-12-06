let readSeq reader =
    let unfold (reader: System.IO.TextReader) =
        match reader.ReadLine() with
        | null -> None
        | line -> Some(line, reader)

    Seq.unfold unfold reader

module String =
    let split pattern string =
        System.Text.RegularExpressions.Regex.Split(string, pattern)
        |> Array.toSeq

    let parseInt (string: string) = System.Convert.ToInt32(string)

module One = // For more information see https://aka.ms/fsharp-console-apps
    let parse: System.IO.TextReader -> int list * int list =
        let fold ((ls, rs) as acc) line =
            line
            |> String.split "\s+"
            |> Seq.map String.parseInt
            |> Seq.toList
            |> function
                | l :: r :: [] -> l :: ls, r :: rs
                | _ -> acc

        readSeq
        >> Seq.fold fold ([], [])
        >> function
            | (ls, rs) -> List.rev ls, List.rev rs

    let one stream =
        let (ls, rs) = parse stream

        List.map2 (fun l r -> abs (l - r)) (List.sort ls) (List.sort rs)
        |> List.sum

    let two stream =
        let (ls, rs) = parse stream
        let freq = rs |> List.countBy id |> Map.ofList

        ls
        |> List.map (fun i ->
            freq
            |> Map.tryFind i
            |> Option.defaultValue 0
            |> (*) i)
        |> List.sum

module Two =
    let parse =
        readSeq
        >> Seq.map (String.split "\s+" >> (Seq.map String.parseInt))

    let one =
        let rec isSafe direction =
            let inRange i = List.contains i [ 1; 2; 3 ]

            function
            | first :: second :: rest ->
                match direction with
                | None when inRange (abs (second - first)) ->
                    isSafe (Some(first < second)) (second :: rest)
                | Some true when inRange (second - first) -> isSafe direction (second :: rest)
                | Some false when inRange (first - second) -> isSafe direction (second :: rest)
                | _ -> false
            | _ -> true

        parse
        >> Seq.filter (Seq.toList >> (isSafe None))
        >> Seq.length



printfn "%A" <| Two.one stdin
