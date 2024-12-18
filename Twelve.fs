module Twelve

type Pt = int * int

type Direction =
    | N
    | E
    | S
    | W

type Plot = { edges: Direction Set; pt: Pt }
type Region = { plant: char; plots: Pt Set }

let directions = Set.ofList [ N; E; S; W ]

let neighbor (x, y) =
    function
    | N -> (x, y - 1)
    | E -> (x + 1, y)
    | S -> (x, y + 1)
    | W -> (x - 1, y)

let opposite =
    function
    | N -> S
    | E -> W
    | S -> N
    | W -> E

let printEdges vertical horizontal garden =
    seq {
        for y in 0 .. (Array2D.length2 garden - 1) do
            for x in 0 .. (Array2D.length1 garden - 1) do
                let pt = (x, y)

                match Set.contains pt vertical, Set.contains pt horizontal with
                | true, true -> printf "+"
                | true, false -> printf "|"
                | false, true -> printf "-"
                | false, false -> printf " "

            printf "\n"
    }
    |> Seq.toList
    |> ignore


let one lines =
    let garden = lines |> Grid.fromLines id

    let findEdges max1 max2 mapPoint =
        let foldColumns edges y =
            let fold (plant, edges) x =
                let pt = mapPoint (x, y)

                match plant, Grid.tryItem pt garden with
                | Some previousPlant, Some currentPlant when previousPlant = currentPlant -> Some currentPlant, edges
                | Some _, Some currentPlant -> Some currentPlant, Set.add pt edges
                | None, Some currentPlant -> Some currentPlant, edges
                | _, None -> None, Set.add pt edges

            seq { 0 .. (max2 garden) - 1 }
            |> Seq.fold fold (None, edges)
            |> snd
            |> Set.union edges

        seq { 0 .. (max1 garden) - 1 } |> Seq.fold foldColumns Set.empty

    let verticalEdges = findEdges Array2D.length1 Array2D.length2 id

    let horizontalEdges = findEdges Array2D.length2 Array2D.length1 (fun (x, y) -> y, x)

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

    let allPlots =
        seq {
            for x in 0 .. (Array2D.length1 garden - 1) do
                for y in 0 .. (Array2D.length2 garden - 1) -> x, y
        }
        |> Set.ofSeq

    let rec gatherRegions regions =
        let unvisited =
            Set.difference allPlots (Set.unionMany (regions |> List.map (fun r -> r.plots)))

        match Set.isEmpty unvisited with
        | true -> regions
        | false ->
            let (x, y) = Seq.head unvisited

            let region =
                gatherRegion
                    { plant = garden[x, y]
                      plots = Set.empty }
                    (x, y)

            gatherRegions (region :: regions)

    let regions = gatherRegions []

    let countEdges region =
        let map (edges, neighborDirection) =
            let fold sum pt =
                sum
                + (edges
                   |> Set.intersect (Set.ofList [ pt; neighbor pt neighborDirection ])
                   |> Set.count)
                + (2
                   - ([ neighbor pt neighborDirection; neighbor pt (opposite neighborDirection) ]
                      |> List.choose (fun pt -> garden |> Grid.tryItem pt)
                      |> List.length))

            region.plots |> Set.fold fold 0

        [ verticalEdges, E; horizontalEdges, S ] |> List.map map |> List.sum

    // regions |> List.iter (fun region -> printfn "plant: %c perimiter: %d\tarea: %d" region.plant (countEdges region)  (Set.count region.plots))
    regions
    |> List.map (fun region -> (countEdges region) * (Set.count region.plots))
    |> List.sum
    |> printfn "%A"

    0

let two lines = 0
