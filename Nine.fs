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

let one: string seq -> int =
    let compact =
        let rec compact compacted files =
            printfn "compacted: %A\nfiles:\n %A" compacted files

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
                    printfn "src: %A\ndest: %A" src dest

                    compact
                        ({ dest with gap = 0 } :: compacted)
                        ({ src with gap = gap - size }
                         :: (List.tail (List.rev sources)))

                // file exactly fits
                | { gap = gap } as dest :: _ when gap = size ->
                    printfn "exact fit"

                    printfn "src: %A\ndest: %A" src dest

                    compact
                        ({ src with size = gap; gap = 0 }
                         :: { dest with gap = 0 } :: compacted)
                        (List.rev sources |> List.tail)
                // file partially fits
                | { gap = gap } as dest :: _ ->
                    printfn "partial fit"

                    printfn "src: %A\ndest: %A" src dest

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
        let mapi i { id = id } = id * i

        (function
        | files ->
            printfn "result: %A" files
            files)
        >> Seq.mapi mapi
        >> Seq.sum

    parse >> Seq.toList >> compact >> checksum

let two lines = 0
