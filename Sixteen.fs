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

type Path =
    { edges: Edge list
      unvisited: Pt Set
      remaining: int }

let two' lines =
    // like dijkstra but keep all paths?
    // at each fork, add another path to the queue
    // if we reach the end, add the path to the list of paths
    // if we have no more places to go from a given path, remove it from the queue

    let maze = parse lines

    let maxScore = dijkstra maze

    let rec loop completed =
        function
        | [] -> completed
        | ({ edges = (current :: _) as path
             unvisited = unvisited
             remaining = remaining } :: paths) ->
            let unvisited = Set.remove current.pt unvisited
            printfn "%d" (List.length paths)

            let exits, neighbors =
                current
                |> getEdges unvisited
                |> List.filter (fun edge -> remaining - edge.cost >= 0)
                |> List.partition (fun { pt = pt } -> pt = maze.exit)

            let completed' = List.map (fun exit -> exit :: path) exits

            let paths' =
                List.map
                    (fun neighbor ->
                        { edges = neighbor :: path
                          unvisited = unvisited
                          remaining = remaining - neighbor.cost })
                    neighbors

            loop (completed' @ completed) (paths' @ paths)
        | _ -> failwith "A path cannot be empty"


    let start =
        { pt = maze.entrance
          direction = E
          cost = 1 }

    [ { edges = [ start ]
        unvisited = Set.add maze.exit maze.vertices
        remaining = maxScore } ]
    |> loop []
    |> List.concat
    |> List.map (fun edge -> edge.pt)
    |> Set.ofList
    |> Set.count

// trying to pilfer https://github.com/mgtezak/Advent_of_Code/blob/master/2024/16/p2.py

module Two =
    open System.Collections.Generic

    type Placement = { direction: Direction; pt: Pt }

    type Path =
        { placement: Placement
          score: int
          vertices: Pt list }

    let canVisit { placement = placement; score = score } visited =
        visited
        |> Map.tryFind placement
        |> function
            | Some prevScore when prevScore < score -> (visited, false)
            | _ -> (visited |> Map.add placement score, true)

    let push path (q: PriorityQueue<Path, int>) =
        q.Enqueue(path, path.score)
        q

    let pop (q: PriorityQueue<Path, int>) = q, q.Dequeue()

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

        let start =
            { placement = { direction = E; pt = maze.entrance }
              vertices = [ maze.entrance ]
              score = 0 }

        let rec loop lowestScore winningPaths visited (q: PriorityQueue<Path, int>) =
            if q.Count = 0 then
                winningPaths
            else
                match pop q with
                | q, { score = score } when lowestScore < score -> winningPaths
                | q,
                  { placement = placement
                    score = score
                    vertices = vertices } when placement.pt = maze.exit ->
                    loop score (vertices :: winningPaths) visited q
                | q, path ->
                    let (visited, mayVisit) = canVisit path visited

                    let fold (visited, q) path =
                        let (visited, mayVisit) = canVisit path visited

                        let q = if mayVisit then push path q else q

                        visited, q

                    let visited, q =
                        if mayVisit then
                            path |> neighbors |> List.fold fold (visited, q)
                        else
                            visited, q

                    loop lowestScore winningPaths visited q

        let q = PriorityQueue()

        let q = push start q

        q |> loop maxInt [] Map.empty |> List.concat |> Set.ofList |> Set.count

let two = Two.two
