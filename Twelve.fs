module Twelve

type Pt = int * int

type Direction =
    | N
    | E
    | S
    | W

type Region = { plant: char; plots: Pt Set }

let directions = Set.ofList [ N; E; S; W ]

let neighbor (x, y) =
    function
    | N -> (x, y - 1)
    | E -> (x + 1, y)
    | S -> (x, y + 1)
    | W -> (x - 1, y)

let gatherRegions garden =
    let allPlots =
        seq {
            for x in 0 .. (Array2D.length1 garden - 1) do
                for y in 0 .. (Array2D.length2 garden - 1) -> x, y
        }
        |> Set.ofSeq

    let rec gatherRegion region pt =
        garden
        |> Grid.tryItem pt
        |> Option.filter ((=) region.plant)
        |> function
            | Some _ ->
                let plots = Set.add pt region.plots

                plots
                |> Set.difference (Set.map (neighbor pt) directions)
                |> Set.fold gatherRegion { region with plots = plots }
            | None -> region

    let rec gatherRegions unvisited regions =
        match Set.isEmpty unvisited with
        | true -> regions
        | false ->
            let (x, y) = Seq.head unvisited

            let region =
                gatherRegion
                    { plant = garden[x, y]
                      plots = Set.empty }
                    (x, y)

            gatherRegions (Set.difference unvisited region.plots) (region :: regions)

    gatherRegions allPlots []

let one: string seq -> int =
    let traceEdges { plots = plots } =
        let fold count plot =
            let isBlocked direction =
                plots |> Set.contains (neighbor plot direction) |> not

            count + (directions |> Set.filter isBlocked |> Set.count)

        Set.fold fold 0 plots

    Grid.fromLines id
    >> gatherRegions
    >> List.map (fun region -> (traceEdges region) * (Set.count region.plots))
    >> List.sum

module Set =
    let tryMin set =
        match Set.isEmpty set with
        | true -> None
        | false -> Some(Set.minElement set)

let two: string seq -> int =
    let findSides edges =
        let rec findSides found lost =
            match Set.tryMin lost with
            | Some((plot, blocked) as edge) ->
                let sidePiece =
                    directions
                    |> Set.map (fun direction -> neighbor plot direction, blocked)
                    |> Set.intersect edges
                    |> Set.add edge

                let (rest, wall) = Set.partition (Set.intersect sidePiece >> Set.isEmpty) found

                let found =
                    match Set.tryMin wall with
                    | None -> sidePiece // new wall
                    | Some wall -> Set.union wall sidePiece // extend wall

                sidePiece |> Set.difference lost |> findSides (Set.add found rest)
            | None -> found

        findSides Set.empty edges


    let filterEdges plots =
        let fold edges plot =
            let isBlocked direction =
                plots |> Set.contains (neighbor plot direction) |> not

            directions
            |> Set.filter isBlocked
            |> Set.map (fun blocked -> plot, blocked)
            |> Set.union edges

        Set.fold fold Set.empty plots

    let countSides { plots = plots } =
        plots |> filterEdges |> findSides |> Seq.length

    Grid.fromLines id
    >> gatherRegions
    >> List.map (fun region ->
        // printfn "plant: %A area: %A\t sides: %A" region.plant (Set.count region.plots) (countSides region)
        (countSides region) * (Set.count region.plots))
    >> List.sum
