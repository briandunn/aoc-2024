﻿let readSeq =
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
                  13, 2, Thirteen.two
                  14, 1, Fourteen.one
                  14, 2, Fourteen.two
                  15, 1, Fifteen.one
                  15, 2, Fifteen.two
                  16, 1, Sixteen.one
                  17, 1, Seventeen.one
                  17, 2, Seventeen.two
                  18, 1, Eighteen.one
                  18, 2, Eighteen.two
                  19, 1, Nineteen.one
                  19, 2, Nineteen.two
                  20, 1, Twenty.one
                  20, 2, Twenty.two
                  21, 1, TwentyOne.one
                  21, 2, TwentyOne.two
                  22, 1, TwentyTwo.one
                  22, 2, TwentyTwo.two
                  23, 1, TwentyThree.one
                  23, 2, TwentyThree.two
                  24, 1, TwentyFour.one
                  24, 2, TwentyFour.two
                  25, 1, TwentyFive.one
                  25, 2, TwentyFive.two ] -> (day, puzzle), f
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
