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

let neighbors { pt = (x, y); direction = direction } =
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

let maxInt = System.Int32.MaxValue

let print maze path =
    let path = List.fold (fun set { pt = pt } -> Set.add pt set) Set.empty path

    seq {
        for row in 0 .. maze.entrance |> snd do
            for col in 0 .. maze.exit |> fst do
                let pt = (col, row)
                if Set.contains pt path then yield "O" else yield " "

            yield "\n"
    }
    |> String.concat ""
    |> printf "%s"

let getEdges q =
    neighbors >> List.filter (fun { pt = pt } -> Set.contains pt q)

let dijkstra maze =
    let rec loop dist q =
        function
        | [] -> Map.find maze.exit dist
        | current :: _ when current.pt = maze.exit -> Map.find maze.exit dist
        | current :: starts ->
            let fold dist neighbor =
                let cost = dist |> Map.find current.pt |> ((+) neighbor.cost)
                let previousCost = dist |> Map.tryFind neighbor.pt |> Option.defaultValue maxInt

                if cost < previousCost then
                    Map.add neighbor.pt cost dist
                else
                    dist

            let q = Set.remove current.pt q
            let neighbors = getEdges q current
            let dist = List.fold fold dist neighbors
            loop dist q (List.sortBy (fun { cost = cost } -> cost) (starts @ neighbors))

    let dist = Map.ofList [ maze.entrance, 0 ]

    [ { pt = maze.entrance
        direction = E
        cost = 1 } ]
    |> loop dist (maze.vertices |> Set.add maze.exit)

let one: string seq -> int = parse >> dijkstra

// type 'a PQ = Map<System.IComparable, 'a list>
module PQ =
    let length pq =  pq |> (Map.values >> List.concat >> List.length)

    let tryPop q =
        if Map.isEmpty q then
            None
        else
            match Map.minKeyValue q with
            | _, [] -> None
            | k, [ head ] -> Some(Map.remove k q, head)
            | k, head :: rest -> Some(Map.add k rest q, head)

    let push priority path =
        let change =
            function
            | Some values -> path :: values
            | None -> [ path ]
            >> Some

        Map.change priority change

// trying to pilfer https://github.com/mgtezak/Advent_of_Code/blob/master/2024/16/p2.py
module Two =
    type Placement = { direction: Direction; pt: Pt }

    type Path =
        { placement: Placement
          score: int
          vertices: Pt list }

    let two lines =
        let maze = parse lines

        let vertices = maze.vertices |> Set.add maze.exit

        let neighbors
            ({ placement = { direction = direction; pt = (x, y) }
               score = score } as path)
            =
            let turn direction =
                { path with
                    placement =
                        { path.placement with
                            direction = direction }
                    score = score + 1000 }

            let straight pt direction =
                if Set.contains pt vertices then
                    [ { placement = { direction = direction; pt = pt }
                        score = score + 1
                        vertices = pt :: path.vertices } ]
                else
                    []

            match direction with
            | N -> straight (x, y - 1) N @ [ turn W; turn E ]
            | S -> straight (x, y + 1) S @ [ turn W; turn E ]
            | E -> straight (x + 1, y) E @ [ turn N; turn S ]
            | W -> straight (x - 1, y) W @ [ turn N; turn S ]

        let foldNeighbor (visited, q) ({ placement = placement; score = score } as path) =
            visited
            |> Map.tryFind placement
            |> function
                | Some prevScore when prevScore < score -> (visited, q)
                | _ -> (Map.add placement score visited, PQ.push path.score path q)

        let start =
            { placement = { direction = E; pt = maze.entrance }
              vertices = [ maze.entrance ]
              score = 0 }

        let rec loop lowestScore winningPaths visited q =
            match PQ.tryPop q with
            | None -> winningPaths
            | Some(_, { score = score }) when lowestScore < score -> winningPaths
            | Some(q,
                   { placement = placement
                     score = score
                     vertices = vertices }) when placement.pt = maze.exit ->
                loop score (vertices :: winningPaths) visited q
            | Some(q, path) ->
                let visited, q = path |> neighbors |> List.fold foldNeighbor (visited, q)
                loop lowestScore winningPaths visited q


        Map.empty
        |> PQ.push start.score start
        |> loop maxInt [] Map.empty
        |> List.concat
        |> List.distinct
        |> List.length

let two = Two.two
