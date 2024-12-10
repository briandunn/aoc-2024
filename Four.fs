module Four

type Direction =
    | N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW

let offSet (x, y) =
    function
    | N -> x, y - 1
    | NE -> x + 1, y - 1
    | E -> x + 1, y
    | SE -> x + 1, y + 1
    | S -> x, y + 1
    | SW -> x - 1, y + 1
    | W -> x - 1, y
    | NW -> x - 1, y - 1

let parse: string seq -> char Grid.Grid = Seq.map String.chars >> Grid.fromSeq

let one lines =
    let countAt grid x y =
        function
        | 'X' ->
            let choose start letter direction =
                let pt = offSet start direction

                match Grid.item pt grid with
                | Some letter' when letter = letter' -> Some(direction, pt)
                | _ -> None

            [ N; NE; E; SE; S; SW; W; NW ]
            |> List.choose (choose (x, y) 'M')
            |> List.choose (fun (direction, start) -> choose start 'A' direction)
            |> List.choose (fun (direction, start) -> choose start 'S' direction)
            |> List.length
        | _ -> 0

    let grid = parse lines

    grid
    |> Grid.mapi (countAt grid)
    |> Grid.flatten
    |> Seq.sum

let two lines =
    let countAt grid x y =
        function
        | 'A' ->
            let filter =
                List.choose (fun direction -> Grid.item (offSet (x, y) direction) grid)
                >> Set.ofList
                >> ((=) (Set.ofList [ 'M'; 'S' ]))

            if [ [ NE; SW ]; [ NW; SE ] ] |> List.forall filter then
                1
            else
                0
        | _ -> 0

    let grid = parse lines

    grid
    |> Grid.mapi (countAt grid)
    |> Grid.flatten
    |> Seq.sum
