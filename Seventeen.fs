module Seventeen

type Register =
    | A
    | B
    | C


type Program =
    { registers: Map<Register, int>
      instructions: int array }

module Regex =
    let matches string regex =
        let regex = System.Text.RegularExpressions.Regex(regex)

        [ for m in regex.Matches(string) do
              for s in (Seq.skip 1 m.Groups) do
                  yield s.Value ]

let parse lines =
    let parseInstructions =
        Seq.head
        >> String.split "Program: "
        >> Seq.skip 1
        >> Seq.head
        >> String.split ","
        >> Seq.map int
        >> Array.ofSeq

    let parseRegisters =

        let fold acc line =
            match Regex.matches line "([A-C]): (\d+)" with
            | [ "A"; v ] -> v |> int |> Map.add A
            | [ "B"; v ] -> v |> int |> Map.add B
            | [ "C"; v ] -> v |> int |> Map.add C
            | _ -> id
            <| acc

        Seq.fold fold Map.empty

    { registers = lines |> Seq.takeWhile ((<>) "") |> parseRegisters
      instructions = lines |> parseInstructions }

let one lines =
    parse lines |> printfn "%A"
    0

let two lines = 0
