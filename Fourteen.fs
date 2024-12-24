module Fourteen

type Robot =
    { position: int * int
      velocity: int * int }

let regex = System.Text.RegularExpressions.Regex("-?\d+")

let parse line =
    match regex.Matches(line) |> Seq.map (fun m -> int m.Value) |> Seq.toArray with
    | [| px; py; vx; vy |] ->
        Some(
            { position = (px, py)
              velocity = (vx, vy) }
        )
    | _ -> None

// let dimensions = (11, 7)
let dimensions = (101, 103)

let (w, h) = dimensions
let mx = w / 2
let my = h / 2

let advance
    ({ position = (px, py)
       velocity = (vx, vy) } as robot)
    =
    let x = (px + vx) % w
    let x = if x < 0 then x + w else x
    let y = (py + vy) % h
    let y = if y < 0 then y + h else y
    { robot with position = x, y }

let print robots =
    let robotMap = robots |> Seq.groupBy (fun r -> r.position) |> Map.ofSeq

    printf "%c[H" (char 0x1B)

    seq {
        for y in 0 .. (h - 1) do
            for x in 0 .. (w - 1) do
                yield
                    match Map.tryFind (x, y) robotMap with
                    | _ when x = mx || y = my -> " "
                    | Some r -> r |> Seq.length |> string
                    | None -> "."

            yield "\n"
    }
    |> Seq.iter (printf "%s")

    robots

module One =
    let quadrant =
        function
        | (x, y) when x < mx && y < my -> Some 0
        | (x, y) when x > mx && y < my -> Some 1
        | (x, y) when x < mx && y > my -> Some 2
        | (x, y) when x > mx && y > my -> Some 3
        | _ -> None

    let one robots =
        let fold robots _ = Seq.map advance robots

        seq { 1..100 }
        |> Seq.fold fold (print robots)
        |> print
        |> Seq.countBy (fun robot -> quadrant robot.position)
        |> Seq.fold
            (fun product ->
                function
                | Some _, count -> count * product
                | None, _ -> product)
            1

module Two =

    let isSymmetrical robots =
        let map =
            robots |> Seq.countBy (fun { position = position } -> position) |> Map.ofSeq

        let reflectX (x, y) = (w - x - 1, y)

        let forall position count =
            map |> Map.tryFind (reflectX position) |> Option.exists ((=) count)

        Map.forall forall map


    let two =
        let unfold robots =
            let next = Seq.map advance robots
            Some(next, next)

        Seq.unfold unfold
        >> Seq.indexed
        >> Seq.skipWhile (snd >> isSymmetrical >> not)
        >> Seq.head
        >> fun (i, robots) ->
            robots |> print |> ignore
            i

let one: string seq -> int = Seq.choose parse >> Seq.toList >> One.one
let two: string seq -> int = Seq.choose parse >> Seq.toList >> Two.two
