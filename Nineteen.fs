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

    let getRemainders =
        let rec loop towels remainders =
            function
            | _ when towels = [] -> remainders
            | [] -> [] :: remainders
            | (color :: rest) as pattern ->
                let fold (towels, remainders) =
                    function
                    | color' :: towel when color = color' -> (towel :: towels, remainders)
                    | [] -> (towels, pattern :: remainders)
                    | _ -> (towels, remainders)

                let towels, remainders = List.fold fold ([], remainders) towels
                loop towels remainders rest

        loop onsen.towels []

    let isPossible pattern =
        let rec loop =
            function
            | []::_ -> true
            | head :: rest -> loop ((getRemainders head) @ rest)
            | [] -> false

        loop [ pattern ]

    // 400 too high
    onsen.patterns |> List.filter isPossible |> List.length

let two lines = 0
