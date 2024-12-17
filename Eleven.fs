module Eleven

let parse =
    Seq.tryExactlyOne
    >> Option.map (String.split "\D+" >> Seq.map int64)
    >> Option.defaultValue Seq.empty

let isEven n = n % 2 = 0

let blinkOne map =
    let inc count =
        function
        | Some n -> Some(n + count)
        | None -> Some count

    let fold acc (stone, count) =
        match stone with
        | 0L -> acc |> Map.change 1L (inc count)
        | n when n |> string |> String.length |> isEven ->
            n
            |> string
            |> String.chars
            |> Seq.splitInto 2
            |> Seq.map (fun chars -> new string (chars) |> int64)
            |> Seq.fold (fun acc stone -> Map.change stone (inc count) acc) acc
        | n -> acc |> Map.change (n * 2024L) (inc count)

    map |> Map.toSeq |> Seq.fold fold Map.empty


let intoMap map =
    let fold map stone =
        let change =
            function
            | Some n -> Some(n + 1L)
            | None -> Some 1L

        Map.change stone change map

    Seq.fold fold map

let toMap = intoMap Map.empty

let blink n =
    let rec blink' n =
        function
        | stones when n = 0 -> stones
        | stones -> blink' (n - 1) (blinkOne stones)

    toMap >> blink' n >> Map.values >> Seq.sum

let one: string seq -> int = parse >> blink 25 >> int

let two: string seq -> int =
    // 1415754763 - too low
    parse
    >> blink 75
    >> (fun count ->
        printfn "%d" count
        0)
