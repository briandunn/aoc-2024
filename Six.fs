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

type Pt = int * int

type Guard = { direction: Direction; location: Pt }

type Grid =
    { obstructions: Set<Pt>
      width: int
      height: int }

type Board = { guard: Guard; grid: Grid }

let parse lines =
    let updateDimensions grid (x, y, _) =
        { grid with
            width = max grid.width (x + 1)
            height = max grid.height (y + 1) }

    let fold ((guard, grid) as acc) cell =
        let grid = updateDimensions grid cell

        match cell with
        | (x, y, '#') ->
            (guard,
             { grid with
                 obstructions = Set.add (x, y) grid.obstructions })
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


let stepForward
    ({ direction = direction
       location = (x, y) } as guard)
    =
    { guard with
        location =
            match direction with
            | N -> (x, y - 1)
            | E -> (x + 1, y)
            | S -> (x, y + 1)
            | W -> (x - 1, y) }

let turnRight ({ direction = direction } as guard) =
    { guard with
        direction =
            match direction with
            | N -> E
            | E -> S
            | S -> W
            | W -> N }

let outOfBounds (x, y) grid =
    x = grid.width || y = grid.height || x < 0 || y < 0

let one: (string seq) -> int =
    let walk { guard = guard; grid = grid } =
        let rec walk' visited guard =
            match stepForward guard with
            | { location = nextLocation } when Set.contains nextLocation grid.obstructions ->
                walk' visited (turnRight guard)
            | { location = location } when outOfBounds location grid -> visited
            | next -> walk' (Set.add next.location visited) next

        walk' Set.empty guard

    parse >> walk >> Set.count

let two lines =
    let { grid = grid; guard = guardOrigin } = parse lines

    let walk obstructions =
        let rec walk' visited guard =
            let visited = Set.add guard visited

            match stepForward guard with
            | next when Set.contains next.location obstructions -> guard |> turnRight |> walk' visited
            | next when (Set.contains next visited) -> (true, visited)
            | next when outOfBounds next.location grid -> (false, visited)
            | next -> walk' visited next

        walk' Set.empty guardOrigin

    let filter obstruction =
        match grid.obstructions |> Set.add obstruction |> walk with
        | (true, _) -> true
        | _ -> false

    let (_, path) = walk grid.obstructions

    path
    |> Set.map (function
        | { location = location } -> location)
    |> Set.remove guardOrigin.location
    |> Set.filter filter
    |> Set.count
// 1655
