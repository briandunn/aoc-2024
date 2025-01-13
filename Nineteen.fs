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

let splitStarts towels (pattern: string)  =
    let fold acc (towel: string) =
        match pattern.Split(towel, 2) with
        | [| ""; rest |] -> rest :: acc
        | _ -> acc

    List.fold fold [] towels

let one lines =
    let onsen = parse lines

    let splitStarts = splitStarts onsen.towels

    let rec loopSplits remainders =
        function
        | [] -> false, remainders
        | "" :: _ -> true, remainders
        | remainder :: rest -> loopSplits (Set.add remainder remainders) rest

    let canBeArranged pattern =
        let rec loop isDone (remainders: string Set) =
            if isDone then
                1
            elif Set.isEmpty remainders then
                0
            else
                let pattern = Set.minElement remainders

                let isDone, remainders' = loopSplits Set.empty (splitStarts pattern)

                loop isDone (remainders |> Set.remove pattern |> Set.union remainders')

        loop false (Set.singleton pattern)

    List.sumBy canBeArranged onsen.patterns

let two lines =
    let onsen = parse lines

    let splitStarts = splitStarts onsen.towels

    let allArrangements pattern =
        let rec loop total remainders =
            if Map.isEmpty remainders then
                total
            else
                let pattern, arrangementCount = Map.minKeyValue remainders
                let change = Option.defaultValue 0L >> ((+)) arrangementCount >> Some

                let fold (total, remainders) =
                    function
                    | "" -> total + arrangementCount, remainders
                    | remainder -> total, Map.change remainder change remainders

                let total, remainders = pattern |> splitStarts |> List.fold fold (total, remainders)

                loop total (Map.remove pattern remainders)

        loop 0L (Map.ofList [ pattern, 1L ])

    List.sumBy allArrangements onsen.patterns |> printfn "%d"
    0
