module Fifteen

type Pt = int * int

type Tile =
    | Wall
    | Robot
    | Box

type Direction =
    | N
    | E
    | S
    | W

type State =
    { map: Map<Pt, Tile>
      moves: Direction seq }

let parse (lines: string seq) =
    let parseMap (lines: string seq) =
        seq {
            for y, line in Seq.indexed lines do
                for x, c in Seq.indexed line do
                    match c with
                    | '#' -> yield ((x, y), Wall)
                    | '@' -> yield ((x, y), Robot)
                    | 'O' -> yield ((x, y), Box)
                    | _ -> ()
        }
        |> Map.ofSeq

    let parseMove =
        function
        | '^' -> Some N
        | '>' -> Some E
        | 'v' -> Some S
        | '<' -> Some W
        | _ -> None

    let map = lines |> Seq.takeWhile (String.length >> ((<>) 0)) |> parseMap
    let moves = lines |> String.concat "" |> Seq.choose parseMove
    { map = map; moves = moves }


let one (lines: string seq) =
    lines |> parse |> printfn "%A"
    0

let two lines = 0
