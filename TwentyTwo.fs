module TwentyTwo

let mix a b = a ^^^ b

/// truncate to 24 bits
let prune a = a &&& 0xFFFFFF

let next a =
    let a = (a <<< 6) |> mix a |> prune
    let a = (a >>> 5) |> mix a |> prune
    a <<< 11 |> mix a |> prune

let one (lines: string seq) =
    let generator n = Some(n, next n)

    lines
    |> Seq.map (int >> (Seq.unfold generator))
    |> Seq.map (Seq.skip 2000 >> Seq.head >> int64)
    |> Seq.sum
    |> printfn "%d"

    0

let two (lines: string seq) =
    let generator n = Some(n, next n)

    let price n = n % 10

    let deltaPrice = Seq.pairwise >> Seq.map (fun (b, a) -> price a - price b, price a)

    let toMap map =
        function
        | [|a, _; b, _; c, _; d, p|] ->
            let key = a, b, c, d
            if Map.containsKey key map then
                map
            else
                Map.add key p map
        | _ -> map

    let maps =
        lines
        |> Seq.map (int >> (Seq.unfold generator))
        |> Seq.map (Seq.take 2001 >> deltaPrice >> Seq.windowed 4 >> Seq.fold toMap Map.empty)
        |> Seq.cache

    let allValues k = Seq.map (Map.tryFind k >> Option.defaultValue 0) maps

    Set.empty
    |> Seq.foldBack (Map.keys >> Set.ofSeq >> Set.union) maps
    |> Seq.map (allValues >> Seq.sum)
    |> Seq.max
