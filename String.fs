module String

let split pattern string =
    System.Text.RegularExpressions.Regex.Split(string, pattern)
    |> Array.toSeq

let parseInt (string: string) = System.Convert.ToInt32(string)

let chars (string: string) = string.ToCharArray() |> Array.toSeq
