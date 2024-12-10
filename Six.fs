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
    x = grid.width
    || y = grid.height
    || x < 0
    || y < 0

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
    // count the points where *if* the guard were to turn right, it would
    // * be obstructed
    // * on a side that has been visited

    // track point and heading when obstructed

    // must check as we go - only visted up to this point
    let { grid = grid; guard = guard } = parse lines

    let walk guard =
        let rec walk' loops obstructed guard =
            let rec wouldLoop =
                stepForward
                >> function
                    | next when Set.contains next obstructed -> true
                    | { location = location } when outOfBounds location grid -> false
                    | next -> wouldLoop next

            match stepForward guard with
            | { location = nextLocation } when Set.contains nextLocation grid.obstructions ->
                walk' loops (Set.add guard obstructed) (turnRight guard)
            | { location = location } when outOfBounds location grid -> loops
            | next when next |> turnRight |> wouldLoop -> walk' (Set.add (stepForward next).location loops) obstructed next
            | next -> walk' loops obstructed next

        walk' Set.empty Set.empty guard

    // 656 too low
    guard |> walk |> Set.count
