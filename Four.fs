module Four

type 'a Grid = 'a array2d

module Grid =

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

    let fromSeq (rows: 'a seq seq) : 'a Grid =
        let rows = rows |> Seq.toArray
        let grid = Array2D.create (Seq.length rows) (rows |> Seq.head |> Seq.length) None

        rows
        |> Seq.iteri (fun i row ->
            row
            |> Seq.iteri (fun j cell -> grid.[i, j] <- Some cell))

        grid |> Array2D.map Option.get

type Direction =
    | N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW

let offSet (x, y) =
    function
    | N -> x, y - 1
    | NE -> x + 1, y - 1
    | E -> x + 1, y
    | SE -> x + 1, y + 1
    | S -> x, y + 1
    | SW -> x - 1, y + 1
    | W -> x - 1, y
    | NW -> x - 1, y - 1

let parse: string seq -> char Grid = Seq.map String.chars >> Grid.fromSeq

let one lines =
    let countAt grid x y =
        function
        | 'X' ->
            let choose start letter direction =
                let pt = offSet start direction

                match Grid.item pt grid with
                | Some letter' when letter = letter' -> Some(direction, pt)
                | _ -> None

            [ N; NE; E; SE; S; SW; W; NW ]
            |> List.choose (choose (x, y) 'M')
            |> List.choose (fun (direction, start) -> choose start 'A' direction)
            |> List.choose (fun (direction, start) -> choose start 'S' direction)
            |> List.length
        | _ -> 0

    let grid = parse lines

    grid
    |> Grid.mapi (countAt grid)
    |> Grid.flatten
    |> Seq.sum

let two lines =
    let countAt grid x y =
        function
        | 'A' ->
            let filter =
                List.choose (fun direction -> Grid.item (offSet (x, y) direction) grid)
                >> Set.ofList
                >> ((=) (Set.ofList [ 'M'; 'S' ]))

            if [ [ NE; SW ]; [ NW; SE ] ] |> List.forall filter then
                1
            else
                0
        | _ -> 0

    let grid = parse lines

    grid
    |> Grid.mapi (countAt grid)
    |> Grid.flatten
    |> Seq.sum
