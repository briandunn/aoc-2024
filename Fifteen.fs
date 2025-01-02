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
    | (Some robot, map) -> { map = map; robot = robot; moves = moves }
    | _ -> failwith "No robot found"

let checksum =
    let gps (x, y) = x + (y * 100)

    let fold sum pt =
        function
        | Wall -> sum
        | Box -> sum + gps pt

    snd >> Map.fold fold 0

let one : string seq -> int =
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

    let move { map = map; robot = robot; moves = moves } =
        let fold (robot, map) move = tryMove robot move map
        Seq.fold fold (robot, Map.remove robot map) moves

    parse >> move >> checksum

let two lines = 0