module Nine

type File = { id: int; size: int; gap: int }

module List =
    let dropLast list =
        match list with
        | [] -> []
        | _ -> list |> List.rev |> List.tail |> List.rev

    let replaceLast last list =
        match list with
        | [] -> []
        | _ ->
            (last :: (list |> List.rev |> List.tail))
            |> List.rev

let parse: string seq -> File seq =
    Seq.tryExactlyOne
    >> Option.map String.chars
    >> Option.defaultValue Seq.empty
    >> Seq.chunkBySize 2
    >> Seq.indexed
    >> Seq.choose (function
        | (id, [| size; space |]) ->
            { id = id
              size = size |> string |> int
              gap = space |> string |> int }
            |> Some
        | (id, [| size |]) ->
            { id = id
              size = size |> string |> int
              gap = 0 }
            |> Some
        | _ -> None)

let one': string seq -> int =
    let compact =
        let rec compact compacted files =
            // printfn "compacted: %A\nfiles:\n %A" compacted files

            match List.rev files with
            | ({ size = size } as src) :: sources ->
                match List.rev sources with
                // no room
                | { gap = gap } as dest :: destinations when gap = 0 ->
                    printfn "no room"
                    compact (dest :: compacted) destinations
                // whole file fits
                | { gap = gap } as dest :: _ when gap > size ->
                    printfn "fit"
                    // printfn "src: %A\ndest: %A" src dest

                    compact
                        ({ dest with gap = 0 } :: compacted)
                        ({ src with gap = gap - size }
                         :: (List.tail (List.rev sources)))

                // file exactly fits
                | { gap = gap } as dest :: _ when gap = size ->
                    printfn "exact fit"

                    // printfn "src: %A\ndest: %A" src dest

                    compact
                        ({ src with size = gap; gap = 0 }
                         :: { dest with gap = 0 } :: compacted)
                        (List.rev sources |> List.tail)
                // file partially fits
                | { gap = gap } as dest :: _ ->
                    printfn "partial fit"

                    // printfn "src: %A\ndest: %A" src dest

                    compact
                        ({ src with size = gap; gap = 0 }
                         :: { dest with gap = 0 } :: compacted)
                        (List.rev (
                            { src with
                                size = size - gap
                                gap = src.gap + gap }
                            :: sources
                         )
                         |> List.tail)
                | [] ->
                    printfn "no more destinations"
                    List.rev compacted @ files
            | [] ->
                printfn "no more sources"
                List.rev compacted @ files

        compact []

    let checksum =
        let fold acc { id = id; size = size } = acc @ List.replicate size (int64 id)

        Seq.fold fold []
        >> Seq.mapi (fun i id -> (int64 i) * id)
        >> Seq.sum

    // 5839967396819 - too low

    parse
    >> Seq.toList
    >> compact
    >> checksum
    >> (fun x ->
        printfn "%d" x
        0)

let one: string seq -> int =
    let toMap =
        let fold (i, map) { id = id; size = size; gap = gap } =
            let fold map (j, id) = Map.add (i + j) id map

            (i + size + gap,
            id
            |> Seq.replicate size
            |> Seq.indexed
            |> Seq.fold fold map)

        Seq.fold fold (0, Map.empty) >> snd

    let nextGap lastGapIndex lastBlock disk =
        seq {
            for i in lastGapIndex + 1 .. lastBlock do
                if not (Map.containsKey i disk) then
                    yield i
        }
        |> Seq.tryHead

    let compact =
        let rec compact' lastGapIndex disk =
            let (srcKey, src) = Map.maxKeyValue disk

            disk
            |> nextGap lastGapIndex srcKey
            |> function
                | Some destKey ->
                    disk
                    |> Map.remove srcKey
                    |> Map.add destKey src
                    |> compact' destKey
                | None -> disk

        compact' 0

    let checksum =
        let fold sum index id = sum + (int64 index) * (int64 id)
        Map.fold fold 0L

    parse
    >> toMap
    >> compact
    >> checksum
    >> printfn "%A"
    >> (fun _ -> 0)

let two lines = 0
