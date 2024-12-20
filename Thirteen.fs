module Thirteen

type Pt = int * int
type Machine = { a: Pt; b: Pt; prize: Pt }

let parse lines =
    let regex = System.Text.RegularExpressions.Regex("(\d+)")

    let parsePoint line =
        match regex.Matches(line) |> Seq.map (fun m -> int m.Value) |> Seq.toArray with
        | [| x; y |] -> Some(x, y)
        | _ -> None

    let parseMachine =
        Array.choose parsePoint
        >> function
            | [| a; b; prize |] -> Some({ a = a; b = b; prize = prize })
            | _ -> None

    lines |> Seq.chunkBySize 4 |> Seq.choose (Array.take 3 >> parseMachine)

let one lines =

    lines |> parse |> printfn "%A"
    0

let two lines = 0
