module Grid

type 'a Grid = 'a array2d
// let flatten : 'a Grid -> 'a array = Array.fold Array.append [||]
let flatten (grid: 'a [,]) = Seq.cast<'a> grid

let mapi = Array2D.mapi

let tryItem (x, y) grid =
    if Array2D.length1 grid > x
       && Array2D.length2 grid > y
       && x >= 0
       && y >= 0 then
        Some(Array2D.get grid x y)
    else
        None

let private indexToCoords index (grid: 'a [,]) =
    ( index / Array2D.length1 grid,index % Array2D.length1 grid)

let filter (predicate: 'a -> bool) (grid: 'a Grid) : (int * int) seq =
    grid
    |> flatten
    |> Seq.indexed
    |> Seq.filter (fun (_, cell) -> predicate cell)
    |> Seq.map (fun (i, _) -> indexToCoords i grid)

let tryFindCoords (predicate: 'a -> bool) (grid: 'a Grid) : (int * int) option =
    grid
    |> flatten
    |> Seq.tryFindIndex predicate
    |> Option.map (fun i -> indexToCoords i grid)

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

    Array2D.init (Array.length rows.[0]) (Array.length rows) (fun x y -> map (rows.[y].[x]))
