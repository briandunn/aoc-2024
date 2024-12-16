module Ten

let printPath grid path =
    let w = path |> List.map fst |> List.max
    let h = path |> List.map snd |> List.max
    let path = Set.ofList path

    [ for y in 0..h ->
          [ for x in 0..w ->
                if path |> Set.contains (x, y) then
                    printf "%d" (Grid.tryItem (x, y) grid |> Option.defaultValue 0)
                else
                    printf " " ]
          |> ignore

          printf "\n" ]
    |> ignore

let one lines =
    let grid = lines |> Grid.fromLines (string >> int)

    let neighbors (x, y) =
        [ x - 1, y
          x + 1, y
          x, y - 1
          x, y + 1 ]

    let paths start =
        let rec paths' complete =
            function
            | (((start, level) :: _) as path) :: paths when level < 9 ->
                let nexts =
                    start
                    |> neighbors
                    |> List.filter (fun neighbor ->
                        grid
                        |> Grid.tryItem neighbor
                        |> Option.filter ((=) (level + 1))
                        |> Option.isSome)


                paths' complete
                    ((List.map (fun next -> (next, (level + 1)) :: path) nexts)
                     @ paths)
            | summit::paths -> paths' (summit :: complete) paths
            | [] -> complete |> Seq.map (List.head) |> Set.ofSeq

        paths' [] [ [ start,0 ] ]

    printfn "%A" grid

    grid
    |> Grid.filter ((=) 0)
    |> Seq.sortBy snd
    // |> Seq.take 1
    |> Seq.map paths
    |> Seq.concat
    |> Seq.length
    |> printfn "%A"
    0

let two lines = 0
