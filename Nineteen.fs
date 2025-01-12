module Nineteen

type Onsen =
    { towels: char list list
      patterns: char list list }

let one lines =
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

    let onsen = parse lines

    let splitStarts =
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

        loop onsen.towels []

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
                    |> splitStarts
                    |> List.fold (fun acc remainder -> Set.add remainder acc) (Set.remove pattern remainders)
                )

        loop (Set.singleton pattern)

    onsen.patterns |> List.filter splitIntoTowels |> List.length

let two lines = 0
