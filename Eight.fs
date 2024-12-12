module Eight

type Pt = int * int

type Grid =
    { antennas: Map<char, Set<Pt>>
      width: int
      height: int }

let rec comb n list =
    match n, list with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | _, (head :: rest) ->
        List.map ((@) [ head ]) (comb (n - 1) rest)
        @ comb n rest

let parse lines =
    let updateDimensions grid x y =
        { grid with
            width = max grid.width (x + 1)
            height = max grid.height (y + 1) }

    let fold grid (x, y, c) =
        let grid = updateDimensions grid x y

        match c with
        | '.' -> grid
        | c ->
            { grid with
                antennas =
                    Map.change
                        c
                        ((function
                         | Some set -> Set.add (x, y) set
                         | None -> Set.singleton (x, y))
                         >> Some)
                        grid.antennas }

    seq {
        for (y, line) in (Seq.indexed lines) do
            for (x, c) in (Seq.indexed line) do
                yield (x, y, c)
    }
    |> Seq.fold
        fold
        { antennas = Map.empty
          width = 0
          height = 0 }

let inBounds grid (x, y) =
    x >= 0
    && x < grid.width
    && y >= 0
    && y < grid.height

let solve antinodes lines =
    let grid = parse lines

    grid.antennas
    |> Map.values
    |> Seq.map (
        Seq.toList
        >> comb 2
        >> List.choose (function
            | [ a; b ] -> b |> antinodes grid a |> Some
            | _ -> None)
    )
    |> Seq.concat
    |> Seq.concat
    |> Set.ofSeq
    |> Seq.length

let one: string seq -> int =
    let antinodes grid (x1, y1) (x2, y2) =
        let dx = 2 * (x1 - x2)
        let dy = 2 * (y1 - y2)

        [ x1 - dx, y1 - dy; x2 + dx, y2 + dy ]
        |> Seq.filter (inBounds grid)

    solve antinodes

let two: string seq -> int =
    let antinodes grid (x1, y1) (x2, y2) =
        let dx = (x1 - x2)
        let dy = (y1 - y2)

        [ Seq.initInfinite id
          |> Seq.map (fun m -> x1 - m * dx, y1 - m * dy)

          Seq.initInfinite id
          |> Seq.map (fun m -> x2 + m * dx, y2 + m * dy) ]
        |> Seq.map (Seq.takeWhile (inBounds grid))
        |> Seq.concat

    // 1113 - too low
    solve antinodes
