module Fifteen

type Pt = int * int

type Tile =
    | Wall
    | Box

type Direction =
    | N
    | E
    | S
    | W

type State =
    { map: Map<Pt, Tile>
      robot: Pt
      moves: Direction seq }

let parse (lines: string seq) =
    let parseMap (lines: string seq) =
        let fold (robot, map) (pt, tile) =
            match tile with
            | Some Wall -> (robot, map |> Map.add pt Wall)
            | Some Box -> (robot, map |> Map.add pt Box)
            | None -> (Some pt, map)

        seq {
            for y, line in Seq.indexed lines do
                for x, c in Seq.indexed line do
                    match c with
                    | '#' -> yield ((x, y), Some Wall)
                    | '@' -> yield ((x, y), None)
                    | 'O' -> yield ((x, y), Some Box)
                    | _ -> ()
        }
        |> Seq.fold fold (None, Map.empty)

    let parseMove =
        function
        | '^' -> Some N
        | '>' -> Some E
        | 'v' -> Some S
        | '<' -> Some W
        | _ -> None

    let map = lines |> Seq.takeWhile (String.length >> ((<>) 0)) |> parseMap
    let moves = lines |> String.concat "" |> Seq.choose parseMove

    match map with
    | (Some robot, map) ->
        { map = map
          robot = robot
          moves = moves }
    | _ -> failwith "No robot found"

let checksum =
    let gps (x, y) = x + (y * 100)

    let fold sum pt =
        function
        | Wall -> sum
        | Box -> sum + gps pt

    snd >> Map.fold fold 0

let one: string seq -> int =
    let print (robot, map) =
        let keys = Map.keys map
        let w = keys |> Seq.map fst |> Seq.max
        let h = keys |> Seq.map snd |> Seq.max

        seq {
            for y in 0..h do
                for x in 0..w do
                    yield
                        if (x, y) = robot then
                            "@"
                        else
                            match Map.tryFind (x, y) map with
                            | Some Wall -> "#"
                            | Some Box -> "O"
                            | None -> "."

                yield "\n"
        }
        |> String.concat ""
        |> printf "%s"

    let nextPosition (x, y) =
        function
        | N -> (x, y - 1)
        | E -> (x + 1, y)
        | S -> (x, y + 1)
        | W -> (x - 1, y)

    let rec tryMove pt direction map =
        let next = nextPosition pt direction

        match Map.tryFind next map with
        | Some Wall -> (pt, map)
        | Some Box ->
            match tryMove next direction map with
            | (pt', map) when pt' <> next -> (next, map |> Map.remove next |> Map.add pt' Box)
            | _ -> (pt, map)
        | _ -> (next, map)

    let move
        { map = map
          robot = robot
          moves = moves }
        =
        let fold (robot, map) move = tryMove robot move map
        Seq.fold fold (robot, map) moves

    parse >> move >> checksum

module Two =
    type Tile' =
        | Wall'
        | Box' of int

    type State' =
        { map: Map<Pt, Tile'>
          robot: Pt
          moves: Direction seq }

    let print ({ robot = robot; map = map }: State') =
        let keys = Map.keys map
        let w = keys |> Seq.map fst |> Seq.max
        let h = keys |> Seq.map snd |> Seq.max

        let fold y (opened, out) x =
            match (x, y) with
            | pt when pt = robot -> (opened, out + "@")
            | pt ->
                match Map.tryFind pt map with
                | Some(Box' id) when Set.contains id opened -> (Set.remove id opened, out + "]")
                | Some(Box' id) -> (Set.add id opened, out + "[")
                | Some Wall' -> (opened, out + "#")
                | None -> (opened, out + ".")

        seq {
            for y in 0..h do
                yield (seq { 0..w } |> Seq.fold (fold y) (Set.empty, "") |> snd) + "\n"
        }
        |> String.concat ""
        |> printf "%s"

    let nextPosition (x, y) =
        function
        | N -> (x, y - 1)
        | E -> (x + 1, y)
        | S -> (x, y + 1)
        | W -> (x - 1, y)

    let moveBox id direction =
        let map =
            function
            | (pt, ((Box' id') as box)) when id' = id -> (nextPosition pt direction, box)
            | x -> x

        Map.toSeq >> Seq.map map >> Map.ofSeq

    let boxCoords id =
        let fold state pt tile =
            match tile with
            | Box' id' when id' = id -> pt :: state
            | _ -> state

        Map.fold fold []

    let rec tryMoveBox id direction map =
        let rec tryClear map =
            function
            | [] -> Some map
            | pt :: pts ->
                let next = nextPosition pt direction

                match Map.tryFind next map with
                | Some Wall' -> None
                | Some(Box' id') when id' <> id -> map |> tryMoveBox id' direction |> Option.bind (fun map -> tryClear map pts)
                | _ -> tryClear map pts

        map |> boxCoords id |> tryClear map |> Option.map (moveBox id direction)

    let moveRobot robot direction map =
        let next = nextPosition robot direction

        match Map.tryFind next map with
        | Some Wall' -> (robot, map)
        | Some(Box' id) ->
            match tryMoveBox id direction map with
            | Some map -> (next, map)
            | None -> (robot, map)
        | None -> (next, map)

    let move
        (({ map = map
            robot = robot
            moves = moves }) as state)
        =
        let fold (robot, map) move = moveRobot robot move map
        // let (robot, map) = Seq.fold fold (robot, map) (Seq.take 1 moves)
        let (robot, map) = Seq.fold fold (robot, map) moves
        { state with robot = robot; map = map }

    let doubleWidth
        (({ map = map
            robot = robot
            moves = moves }): State)
        : State' =
        let doubleX (x, y) = (x * 2, y)

        let fold (id, map) pt =
            let ((x, y) as pt) = doubleX pt

            function
            | Wall -> (id, map |> Map.add pt Wall' |> Map.add (x + 1, y) Wall')
            | Box -> (id + 1, map |> Map.add pt (Box' id) |> Map.add (x + 1, y) (Box' id))

        { map = map |> Map.fold fold (0, Map.empty) |> snd
          robot = doubleX robot
          moves = moves }

    let checksum {map = map} =
        let gps (x, y) = x + (y * 100)

        let fold map pt =
            let change =
                function
                | Some pt' -> min pt pt'
                | None -> pt
                >> Some

            function
            | Wall' -> map
            | Box' id -> Map.change id change map

        map |> Map.fold fold Map.empty |> Map.values |> Seq.sumBy gps

    let two lines =
        let state = lines |> parse |> doubleWidth

        state |> move |> checksum

let two = Two.two
