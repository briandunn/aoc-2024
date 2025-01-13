module Nineteen

type Onsen =
    { towels: string list
      patterns: string list }

let parse lines =

    let parseTowels = String.split ", " >> Seq.toList

    { towels =
        lines
        |> Seq.takeWhile ((<>) "")
        |> Seq.tryHead
        |> Option.map parseTowels
        |> Option.defaultValue []
      patterns = lines |> Seq.tail |> Seq.toList }

let one lines =
    let onsen = parse lines

    let splitStarts (pattern: string) : string list =
        let fold acc (towel: string) =
            match pattern.Split(towel, 2) with
            | [| ""; rest |] -> rest :: acc
            | _ -> acc

        List.fold fold [] onsen.towels

    let splitIntoTowels pattern =
        let rec loop (remainders: string Set) =
            if Set.contains "" remainders then
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

let two lines =
    let onsen = parse lines

    let splitStarts (pattern: string) : (string * string) list =
        let fold acc (towel: string) =
            match pattern.Split(towel, 2) with
            | [| ""; rest |] -> (towel, rest) :: acc
            | _ -> acc

        List.fold fold [] onsen.towels


    let allArrangements pattern =
        printfn "%A" pattern

        let rec loop remainders =
            if Map.isEmpty remainders then
                []
            elif remainders |> Map.keys |> Seq.tryExactlyOne = Some("") then
                Map.find "" remainders
            else
                let pattern, arrangements = remainders |> Map.remove "" |> Map.minKeyValue

                let fold acc ((towel, remainder): string * string) =
                    let arrangements =
                        match arrangements with
                        | [] -> [ [ towel ] ]
                        | arrangements -> List.map (fun arrangement -> towel :: arrangement) arrangements

                    let change =
                        function
                        | Some previousArrangements -> arrangements @ previousArrangements
                        | None -> arrangements
                        >> Some

                    Map.change remainder change acc

                loop (pattern |> splitStarts |> List.fold fold remainders |> Map.remove pattern)

        loop (Map.ofList [ pattern, [] ]) |> List.map List.rev

    List.sumBy (allArrangements >> List.length) onsen.patterns
