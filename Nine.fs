module Nine

type File = { id: int; size: int; gap: int }

module List =
    let dropLast list =
        match list with
        | [] -> []
        | _ -> list |> List.rev |> List.tail |> List.rev

    let replaceLast last list =
        match list with
        | [] -> [ ]
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

let one: string seq -> int =
    let rec compact files =
        match List.tryLast files with
        | Some ({ size = size } as src) ->
            match files with
            // no room
            | { gap = gap } as dest :: destRest when gap = 0 ->
                printfn "no room"
                dest :: (compact destRest)
            // whole file fits
            | { gap = gap } as dest :: destRest when gap >= size ->
                printfn "fit %A" (src, files)

                { dest with gap = 0 }
                :: (({ src with gap = gap - size }
                     :: (List.dropLast destRest))
                    |> compact)
            // file partially fits
            | { gap = gap } as dest :: destRest ->
                printfn "partial fit %A" (src, files)

                { dest with gap = 0 }
                :: { src with size = gap; gap = 0 }
                   :: compact (
                       List.replaceLast
                           { src with
                               size = size - gap
                               gap = src.gap + gap }
                           destRest
                   )
            | [] -> []
        | None -> []

    let checksum =
        let mapi i { id = id } = id * i

        (function
        | files ->
            printfn "result: %A" files
            files)
        >> Seq.mapi mapi
        >> Seq.sum

    parse >> Seq.toList >> compact >> checksum

let two lines = 0
