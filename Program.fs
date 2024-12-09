﻿let readSeq reader =
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

    let inRange i = List.contains i [ 1; 2; 3 ]

    let one =
        let rec isSafe =
            function
            | (first :: second :: _) as levels -> isSafe' (first < second) levels
            | _ -> true // one level

        and isSafe' ascending =
            function
            | first :: second :: rest when ascending && inRange (second - first) -> isSafe' ascending (second :: rest)
            | first :: second :: rest when not ascending && inRange (first - second) ->
                isSafe' ascending (second :: rest)
            | [ _ ] -> true
            | _ -> false

        parse
        >> Seq.filter (Seq.toList >> isSafe)
        >> Seq.length

    type Dampened =
        | First
        | Second

    let two =
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

module Three =
    let one =
        let parse line =
            let map (m: System.Text.RegularExpressions.Match) =
                String.parseInt m.Groups.[1].Value, String.parseInt m.Groups.[2].Value

            System
                .Text
                .RegularExpressions
                .Regex("mul\((\d+),(\d+)\)")
                .Matches(line)
            |> Seq.map map

        readSeq
        >> Seq.map (parse >> Seq.map (fun (x, y) -> x * y) >> Seq.sum)
        >> Seq.sum

    type Instruction =
        | Mul of int * int
        | Do
        | Don't

    let two stream =
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

            System
                .Text
                .RegularExpressions
                .Regex("(?<mul>mul\((\d+),(\d+)\))|(?<do>do\(\))|(?<dont>don't\(\))")
                .Matches(line)
            |> Seq.choose map

        let foldAll =
            let foldLine (enabled, sum) =
                function
                | Mul (x, y) when enabled -> (true, sum + x * y)
                | Mul _ -> (false, sum)
                | Do -> (true, sum)
                | Don't -> (false, sum)

            Seq.fold foldLine

        stream
        |> readSeq
        |> Seq.map parse
        |> Seq.fold foldAll (true, 0)
        |> function
            | (_, sum) -> sum

[<EntryPoint>]
let main args =
    let puzzles =
        seq {
            for day, puzzle, f in
                [ 1, 1, One.one
                  1, 2, One.two
                  2, 1, Two.one
                  2, 2, Two.two
                  3, 1, Three.one
                  3, 2, Three.two ] -> (day, puzzle), f
        }
        |> Map.ofSeq

    args
    |> Seq.map String.parseInt
    |> Seq.toList
    |> function
        | [ day; puzzle ] ->
            puzzles
            |> Map.tryFind (day, puzzle)
            |> function
                | Some f ->
                    stdin |> f |> printfn "%A"
                    0
                | None ->
                    printfn "No puzzle %d for day %d" puzzle day
                    1

        | m ->
            printfn "invalid input: %A" m
            printfn "Usage: dotnet run <day> <puzzle> < input.txt"
            1
