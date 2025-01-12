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
        printfn "pattern: %A" pattern

        let rec loop (finished: Towel list list) : (Towel list * Pattern) list -> (Towel list) list =
            function
            | [] -> finished
            | (towels, []) :: rest -> loop (towels :: finished) rest
            // | (towels, []) :: rest -> towels :: finished
            | (towels, pattern) :: rest ->
                // printfn "%A" (List.length rest, pattern)
                loop
                    finished
                    (pattern
                     |> splitStarts
                     |> List.fold (fun acc (towel, remainder) -> (towel :: towels, remainder) :: acc) []
                     |> List.append rest)

        loop [] [ [], pattern ] |> List.map (List.rev)

    // 400 too high
    onsen.towels |> List.sort |> List.iter (printfn "%A")

    onsen.patterns
    |> List.skip 5
    |> List.take 1
    // |> fun pattern -> printfn "%A" pattern; pattern
    |> List.map (List.skip 30)
    |> List.filter (splitIntoTowels >> List.isEmpty >> not)
    |> List.length


let one' lines =
    let parseTowels =
        String.split ", "
        >> Seq.map (sprintf "(?:%s)")
        >> String.concat "|"
        >> sprintf "^(?:%s)*$"

    let regexp s = System.Text.RegularExpressions.Regex(s)

    let towels = lines |> Seq.takeWhile ((<>) "") |> Seq.head |> parseTowels |> regexp

    printfn "%A" towels

    lines |> Seq.tail |> Seq.filter (fun s -> towels.IsMatch(s)) |> Seq.length

let two lines = 0
