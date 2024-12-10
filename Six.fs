module Six

type Direction =
    | N
    | E
    | S
    | W

type Tile =
    | Obstruction
    | Open
    | Guard of Direction

let parse' lines =
    lines
    |> Grid.fromLines (function
        | '#' -> Obstruction
        | '.' -> Open
        | '^' -> Guard N
        | '>' -> Guard E
        | '<' -> Guard W
        | c -> failwith (sprintf "Invalid character %c" c))

type Pt = int * int

type Guard = { direction: Direction; location: Pt }

type Grid =
    { obstructions: Set<Pt>
      width: int
      height: int }

type Board = { guard: Guard; grid: Grid }

let parse lines =
    let updateDimensions (guard, grid) ((x, y, _) as cell) =
        (guard,
         { grid with
             width = max grid.width (x + 1)
             height = max grid.height (y + 1) }),
        cell

    let fold acc cell =
        let (((guard, grid) as acc), cell) = updateDimensions acc cell

        match cell with
        | (x, y, '#') -> (guard, { grid with obstructions = Set.add (x, y) grid.obstructions })
        | (x, y, '^') -> (Some { location = (x, y); direction = N }, grid)
        | _ -> acc

    seq {
        for (y, line) in (Seq.indexed lines) do
            for (x, c) in (Seq.indexed line) do
                yield (x, y, c)
    }
    |> Seq.fold
        fold
        (None,
         { obstructions = Set.empty
           width = 0
           height = 0 })
    |> function
        | (Some guard, grid) -> { guard = guard; grid = grid }
        | _ -> failwith "No guard found"

let one lines =
    let grid = parse lines

    grid |> printfn "%A"

    0

let two lines = 0
