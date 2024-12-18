module Twelve

type Pt = int * int

type Direction =
    | N
    | E
    | S
    | W

type Plot = { edges: Direction Set; pt: Pt }
type Region = { plant: char; plots: Plot list }

let directions = Set.ofList [ N; E; S; W ]

let neighbor (x, y) =
    function
    | N -> (x, y - 1)
    | E -> (x + 1, y)
    | S -> (x, y + 1)
    | W -> (x - 1, y)

let one lines =
    let garden = lines |> Grid.fromLines id

    let rec visit =
        function
        | { plant = p
            plots = { edges = edges; pt = pt } as plot :: plots } as region :: regions ->
            let fold (plots, edges, regions) d =
                let pt' = neighbor pt d

                match Grid.tryItem pt' garden with
                | Some c when c = p -> { edges = Set.empty; pt = pt' } :: plots, edges, regions
                | Some c ->
                     plots,
                     Set.add d edges,
                     { plant = c; plots = [{edges = Set.singleton (opposite d); pt = pt'}] } :: regions
                | None -> plots, edges, regions


            Set.fold fold (plots, edges, regions) directions


            region (Set.add pt visited)
        | regions -> regions


    garden
    |> Grid.tryItem (0, 0)
    |> Option.map (fun c ->
        visit
            [ { plant = c
                plots =
                  [ { edges = Set.ofList [ N; W ]
                      pt = (0, 0) } ] } ])
    |> printfn "%A"

    0

let two lines = 0
