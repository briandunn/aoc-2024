module Nineteen

type Color =
    | White
    | Blue
    | Black
    | Red
    | Green

type Towel = Color list

type Pattern = Color list

type Onsen =
    { towels: Towel list
      patterns: Pattern list }

let one lines =
    let parse lines =
        let parsePattern pattern =
            seq {
                for c in pattern do
                    match c with
                    | 'w' -> yield White
                    | 'u' -> yield Blue
                    | 'b' -> yield Black
                    | 'r' -> yield Red
                    | 'g' -> yield Green
                    | _ -> ()
            }
            |> Seq.toList

        let parseTowels = String.split ", " >> Seq.map parsePattern >> List.ofSeq

        { towels =
            lines
            |> Seq.takeWhile ((<>) "")
            |> Seq.tryHead
            |> Option.map parseTowels
            |> Option.defaultValue []
          patterns = lines |> Seq.tail |> Seq.map parsePattern |> Seq.toList }

    let onsen = parse lines

    let splitStarts: Pattern -> (Towel * Pattern) list =
        let rec loop towels remainders =
            function
            | _ when towels = [] -> remainders
            | [] ->
                (List.fold
                    (fun acc ->
                        function
                        | (_, []) as remainder -> remainder :: acc
                        | _ -> acc)
                    []
                    towels)
                @ remainders
            | (color :: rest) as pattern ->
                let fold (towels, remainders) =
                    function
                    | (original, color' :: towel) when color = color' -> ((original, towel) :: towels, remainders)
                    | (original, []) -> (towels, (original, pattern) :: remainders)
                    | _ -> (towels, remainders)

                let towels, remainders = List.fold fold ([], remainders) towels
                loop towels remainders rest

        loop (List.zip onsen.towels onsen.towels) []

    let splitIntoTowels (pattern: Pattern) : (Towel list) list =
        let rec loop finished remainders =
            if Map.isEmpty remainders then
                finished
            else
                match Map.minKeyValue remainders with
                | ([], towels) -> loop (towels :: finished) (Map.remove [] remainders)
                | (pattern, towels) ->
                    loop
                        finished
                        (pattern
                         |> splitStarts
                         |> List.fold
                             (fun acc (towel, remainder) -> Map.add remainder (towel :: towels) acc)
                             (Map.remove pattern remainders))

        loop [] (Map.ofList [ pattern, [] ]) |> List.map (List.rev)

    onsen.patterns
    |> List.filter (splitIntoTowels >> List.isEmpty >> not)
    |> List.length

let two lines = 0
