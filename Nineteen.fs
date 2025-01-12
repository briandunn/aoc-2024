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
                    |> List.fold (fun acc (_, remainder) -> Set.add remainder acc) (Set.remove pattern remainders)
                )

        loop (Set.singleton pattern)

    onsen.patterns |> List.filter splitIntoTowels |> List.length

let two lines = 0
