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
        [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]

    let paths start =
        let rec paths' complete =
            function
            | (((start, level) :: _) as path) :: paths when level < 9 ->
                let nexts =
                    start
                    |> neighbors
                    |> List.choose (fun neighbor ->
                        grid
                        |> Grid.tryItem neighbor
                        |> Option.filter ((=) (level + 1))
                        |> Option.map (fun _ -> neighbor))


                paths' complete ((List.map (fun next -> (next, (level + 1)) :: path) nexts) @ paths)
            | summit :: paths -> paths' (summit :: complete) paths
            | [] -> complete |> Seq.map (List.head) |> Set.ofSeq

        paths' [] [ [ start, 0 ] ]

    grid |> Grid.filter ((=) 0) |> Seq.map paths |> Seq.concat |> Seq.length

let two lines =
    let grid = lines |> Grid.fromLines (string >> int)

    let neighbors (x, y) =
        [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]

    let paths start =
        let rec paths' complete =
            function
            | (((start, level) :: _) as path) :: paths when level < 9 ->
                let choose neighbor =
                    let nextLevel = level + 1

                    grid
                    |> Grid.tryItem neighbor
                    |> Option.filter ((=) nextLevel)
                    |> Option.map (fun _ -> (neighbor, nextLevel) :: path)

                start |> neighbors |> List.choose choose |> List.append paths |> paths' complete
            | summit :: paths -> paths' (summit :: complete) paths
            | [] -> complete

        paths' [] [ [ start, 0 ] ]

    grid |> Grid.filter ((=) 0) |> Seq.map (paths >> Seq.length) |> Seq.sum
