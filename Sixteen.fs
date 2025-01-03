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

let one lines =
    let maze = parse lines

    let buildPath history =
        // history |> Seq.iter (printfn "%A")

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

    let getEdges q =
        neighbors >> List.filter (fun { pt = pt } -> Set.contains pt q)

    let rec loop starts dist prev q =
        if Set.isEmpty q then
            prev
        else
            match starts with
            | [] -> prev
            | current :: starts ->
                let q = Set.remove current.pt q

                let fold (dist, prev) neighbor =
                    let alt =
                        dist
                        |> Map.tryFind current.pt
                        |> Option.defaultValue maxInt
                        |> ((+) neighbor.cost)

                    match Map.tryFind neighbor.pt dist with
                    | Some alt' when alt > alt' -> dist, prev
                    | _ -> Map.add neighbor.pt alt dist, Map.add neighbor.pt current prev

                let neighbors = getEdges q current
                if List.length neighbors > 1 then
                  printfn "neighbors: %A\ncurrent: %A" neighbors current

                let dist, prev = List.fold fold (dist, prev) neighbors

                loop (starts @ neighbors) dist prev q


    let dist = Map.ofList [ maze.entrance, 0 ]

    maze.vertices
    |> Set.add maze.exit
    |> loop
        [ { pt = maze.entrance
            direction = E
            cost = 0 } ]
        dist
        Map.empty
    |> buildPath
    |> print maze

    // |> List.fold (fun acc edge -> acc + edge.cost) 0
    0

let two lines = 0
