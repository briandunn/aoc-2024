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

let one lines =
    let maze = parse lines

    let buildPath history =
        match Map.tryFind maze.exit history with
        | Some edge ->
            let rec loop =
                function
                | (head :: _) as path ->
                    match Map.tryFind head.pt history with
                    | Some edge when edge.pt <> maze.entrance -> loop (edge :: path)
                    | Some entrance -> entrance :: path
                    | _ -> path
                | [] -> []

            loop [ edge ]
        | None -> []

    let rec loop dist prev q =
        function
        | [] -> prev
        | current :: starts ->
            // System.Console.Clear()
            // print maze (prev |> Map.values |> List.ofSeq)
            // System.Threading.Thread.Sleep(10)
            let q = Set.remove current.pt q

            let fold (dist, prev) neighbor =
                let cost =
                    dist
                    |> Map.tryFind current.pt
                    |> Option.defaultValue maxInt
                    |> ((+) neighbor.cost)

                match Map.tryFind neighbor.pt dist with
                | Some previousCost when (previousCost + 3) < cost -> dist, prev
                | _ -> Map.add neighbor.pt cost dist, Map.add neighbor.pt current prev

            let neighbors = getEdges q current

            let dist, prev = List.fold fold (dist, prev) neighbors

            loop dist prev q (List.sortBy (fun { cost = cost } -> cost) (starts @ neighbors))


    let dist = Map.ofList [ maze.entrance, 0 ]

    [ { pt = maze.entrance
        direction = E
        cost = 1 } ]
    |> loop dist Map.empty (maze.vertices |> Set.add maze.exit)
    |> buildPath
    |> List.fold (fun acc edge -> acc + edge.cost) 0

let two lines =
    // like djistra but keep all paths?
    // at each fork, add another path to the queue
    // if we reach the end, add the path to the list of paths
    // if we have no more places to go from a given path, remove it from the queue

    let maze = parse lines

    let rec loop completed =
        function
        | [] -> completed
        | ((current :: path, unvisited) :: paths) ->
            let exits, neighbors =
                current
                |> getEdges unvisited
                |> List.partition (fun { pt = pt } -> pt = maze.exit)

            let completed' = exits |> List.map (fun exit -> exit :: current :: path)

            let paths' =
                neighbors
                |> List.map (fun neighbor -> neighbor :: current :: path, Set.remove current.pt unvisited)

            loop (completed' @ completed) (paths' @ paths)


    let start =
        { pt = maze.entrance
          direction = E
          cost = 1 }

    loop [] [ [start], (Set.add maze.exit maze.vertices) ] |> List.iter (print maze)


    0
