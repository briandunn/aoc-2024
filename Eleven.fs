module Eleven

let parse =
    Seq.tryExactlyOne
    >> Option.map (String.split "\D+" >> Seq.map int64)
    >> Option.defaultValue Seq.empty

let isEven n = n % 2 = 0

let blinkOne =
    function
    | 0L -> Seq.singleton 1L
    | n when n |> string |> String.length |> isEven ->
        n
        |> string
        |> String.chars
        |> Seq.splitInto 2
        |> Seq.map (fun chars -> new string (chars) |> int64)
    | n -> Seq.singleton (n * 2024L)

let blink n =
    let rec blink' n =
        function
        | stones when n = 0 -> stones
        | stones ->
            stones |> Seq.map blinkOne |> Seq.concat |> blink' (n - 1)

    blink' n

let one: string seq -> int = parse >> blink 25 >> Seq.length

// blinkOne is deterministic and must loop at some point
let two lines =
    let stones = parse lines

    stones |> blink 75 |> Seq.length
