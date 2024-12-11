module Eight

type Grid =
    { antennas: Map<char, Set<int * int>>
      width: int
      height: int }

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

let one lines =

    lines |> parse |> printfn "%A"
    // find all pairs of antennas of same frequency
    0

let two lines = 0
