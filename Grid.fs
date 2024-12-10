module Grid

type 'a Grid = 'a array2d
// let flatten : 'a Grid -> 'a array = Array.fold Array.append [||]
let flatten (grid: 'a [,]) = Seq.cast<'a> grid

let mapi = Array2D.mapi

let item (x, y) grid =
    if Array2D.length1 grid > x
       && Array2D.length2 grid > y
       && x >= 0
       && y >= 0 then
        Some(Array2D.get grid x y)
    else
        None

let tryFindCoords(predicate: 'a -> bool) (grid: 'a Grid) : (int*int) option =
    grid |>Seq.cast<'a> |> Seq.tryFindIndex predicate |> Option.map (fun i -> (i / Array2D.length2 grid, i % Array2D.length2 grid))

let fromSeq (rows: 'a seq seq) : 'a Grid =
    let rows = rows |> Seq.toArray
    let grid = Array2D.create (Seq.length rows) (rows |> Seq.head |> Seq.length) None

    rows
    |> Seq.iteri (fun i row ->
        row
        |> Seq.iteri (fun j cell -> grid.[i, j] <- Some cell))

    grid |> Array2D.map Option.get

let fromLines (map: char -> 'a) (lines: string seq) : 'a Grid =
    let rows =
        lines
        |> Seq.map (fun line -> line.ToCharArray())
        |> Seq.toArray

    Array2D.init (Array.length rows) (Array.length rows.[0]) (fun i j -> map (rows.[i].[j]))
