let readSeq =
    let unfold (reader: System.IO.TextReader) =
        match reader.ReadLine() with
        | null -> None
        | line -> Some(line, reader)

    Seq.unfold unfold

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
                  3, 2, Three.two
                  4, 1, Four.one
                  4, 2, Four.two
                  5, 1, Five.one
                  5, 2, Five.two
                  6, 1, Six.one
                  6, 2, Six.two ] -> (day, puzzle), f
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
                    stdin |> readSeq |> f |> printfn "%A"
                    0
                | None ->
                    printfn "No puzzle %d for day %d" puzzle day
                    1

        | m ->
            printfn "invalid input: %A" m
            printfn "Usage: dotnet run <day> <puzzle> < input.txt"
            1
