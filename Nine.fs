module Nine

type File = { id: int; size: int; space: int }

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
              space = space |> string |> int }
            |> Some
        | _ -> None)

let one lines =
    lines |> parse |> printfn "%A"

    0

let two lines = 0
