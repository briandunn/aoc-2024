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
                  6, 2, Six.two
                  7, 1, Seven.one
                  7, 2, Seven.two
                  8, 1, Eight.one
                  8, 2, Eight.two
                  9, 1, Nine.one
                  9, 2, Nine.two
                  10, 1, Ten.one
                  10, 2, Ten.two
                  11, 1, Eleven.one
                  11, 2, Eleven.two
                  12, 1, Twelve.one
                  12, 2, Twelve.two
                  13, 1, Thirteen.one
                  13, 2, Thirteen.two ] -> (day, puzzle), f
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
