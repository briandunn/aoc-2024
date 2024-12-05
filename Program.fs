let readSeq reader =
    let unfold (reader: System.IO.TextReader) =
        match reader.ReadLine() with
        | null -> None
        | line -> Some(line, reader)

    Seq.unfold unfold reader

module Regex =
    let split pattern string =
        System.Text.RegularExpressions.Regex.Split(string, pattern)
        |> Array.toSeq

module One = // For more information see https://aka.ms/fsharp-console-apps
    let parse =
      let fold ((ls, rs) as acc) line =
        line
        |> Regex.split "\s+"
        |> Seq.map System.Convert.ToInt32
        |> Seq.toList
        |> function
            | l :: r :: [] -> l :: ls, r :: rs
            | _ -> acc
      readSeq >> Seq.fold fold ([], []) >> function | (ls, rs) -> List.rev ls, List.rev rs

    let one stream =
      let (ls, rs) = parse stream
      List.map2 (fun l r -> abs (l - r) ) (List.sort ls) (List.sort rs)
      |> List.sum

printfn "%A" <| One.one stdin
