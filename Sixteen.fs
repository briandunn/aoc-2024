module Sixteen

type Pt = int * int

type Maze =
    { vertices: Pt Set
      entrance: Pt
      exit: Pt }

type Direction =
    | N
    | E
    | S
    | W

type Edge =
    { pt: Pt
      direction: Direction
      cost: int }

let parse lines =
    let fold maze =
        function
        | '.', pt ->
            { maze with
                vertices = Set.add pt maze.vertices }
        | 'S', pt -> { maze with entrance = pt }
        | 'E', pt -> { maze with exit = pt }
        | _ -> failwith "Invalid character"

    seq {
        for y, line in Seq.indexed lines do
            for x, char in Seq.indexed line do
                match char with
                | '#' -> ()
                | _ -> yield char, (x, y)
    }
    |> Seq.fold
        fold
        { vertices = Set.empty
          entrance = (-1, -1)
          exit = (-1, -1) }

let neighbors ((x, y), direction) =
    match direction with
    | N ->
        [ { pt = (x, y - 1)
            direction = N
            cost = 1 }
          { pt = (x - 1, y)
            direction = W
            cost = 1001 }
          { pt = (x + 1, y)
            direction = E
            cost = 1001 } ]
    | S ->
        [ { pt = (x, y + 1)
            direction = S
            cost = 1 }
          { pt = (x - 1, y)
            direction = W
            cost = 1001 }
          { pt = (x + 1, y)
            direction = E
            cost = 1001 } ]
    | W ->
        [ { pt = (x - 1, y)
            direction = W
            cost = 1 }
          { pt = (x, y - 1)
            direction = N
            cost = 1001 }
          { pt = (x, y + 1)
            direction = S
            cost = 1001 } ]
    | E ->
        [ { pt = (x + 1, y)
            direction = E
            cost = 1 }
          { pt = (x, y - 1)
            direction = N
            cost = 1001 }
          { pt = (x, y + 1)
            direction = S
            cost = 1001 } ]
    |> Set.ofList

let maxInt = System.Int32.MaxValue

let one lines =
    let maze = parse lines

    let getEdges q =
        neighbors >> Set.filter (fun { pt = pt } -> Set.contains pt q)

    let rec loop current dist prev q =
        let edges = getEdges q current
        printfn "%A" edges

        if Set.isEmpty edges then
            dist, prev
        else
            let edge = Seq.minBy (fun { cost = cost } -> cost) edges
            let q = Set.remove edge.pt q

            let fold (dist, prev) neighbor =
                let alt =
                    dist |> Map.tryFind edge.pt |> Option.defaultValue maxInt |> ((+) neighbor.cost)

                match Map.tryFind neighbor.pt dist with
                | Some alt' when alt >= alt' -> dist, prev
                | _ -> Map.add neighbor.pt alt dist, Map.add neighbor edge prev

            let dist, prev =
                (edge.pt, edge.direction) |> getEdges q |> Set.fold fold (dist, prev)

            loop (edge.pt, edge.direction) dist prev q


    let dist = Map.ofList [ maze.entrance, 0 ]

    loop (maze.entrance, E) dist Map.empty maze.vertices |> printfn "%A"
    0

let two lines = 0
