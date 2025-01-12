module Nineteen

type Onsen =
    { towels: char list list
      patterns: char list list }

let parse lines =

    let parseCharList = Seq.map (String.chars >> List.ofSeq) >> List.ofSeq

    let parseTowels = String.split ", " >> parseCharList

    { towels =
        lines
        |> Seq.takeWhile ((<>) "")
        |> Seq.tryHead
        |> Option.map parseTowels
        |> Option.defaultValue []
      patterns = lines |> Seq.tail |> parseCharList }

let splitStarts towels =
    let rec loop towels remainders =
        function
        | _ when towels = [] -> remainders
        | [] ->
            (List.fold
                (fun acc ->
                    function
                    | [] -> [] :: acc
                    | _ -> acc)
                remainders
                towels)
        | (color :: rest) as pattern ->
            let fold (towels, remainders) =
                function
                | color' :: towel when color = color' -> towel :: towels, remainders
                | [] -> towels, pattern :: remainders
                | _ -> towels, remainders

            let towels, remainders = List.fold fold ([], remainders) towels
            loop towels remainders rest

    loop towels []


let one lines =

    let onsen = parse lines

    let splitIntoTowels pattern =
        let rec loop remainders =
            if Set.contains [] remainders then
                true
            elif Set.isEmpty remainders then
                false
            else
                let pattern = Set.minElement remainders

                loop (
                    pattern
                    |> splitStarts onsen.towels
                    |> List.fold (fun acc remainder -> Set.add remainder acc) (Set.remove pattern remainders)
                )

        loop (Set.singleton pattern)

    onsen.patterns |> List.filter splitIntoTowels |> List.length

let two lines =
    let onsen = parse lines

    let countCombinations pattern =
        printfn "%A" pattern
        let rec loop remainders =
            Map.iter (printfn "%A %d") remainders

            if Map.isEmpty remainders then
                0
            elif remainders |> Map.keys |> Seq.tryExactlyOne = Some([]) then
                Map.find [] remainders
            else
                let pattern, count = Map.minKeyValue remainders

                let change =
                    function
                    // | Some prevCount -> Some(prevCount + count)
                    | Some prevCount -> Some(prevCount + 1)
                    | None -> Some (count + 1)

                loop (
                    pattern
                    |> splitStarts onsen.towels
                    |> List.fold (fun acc remainder -> Map.change remainder change acc) remainders
                    |> Map.remove pattern
                )

        loop (Map.ofList [ pattern, 0 ])

    List.sumBy countCombinations (List.take 1 onsen.patterns)// onsen.patterns
