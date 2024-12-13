module Nine

type File = { id: int; size: int; gap: int }

let parse: string seq -> File seq =
    Seq.tryExactlyOne
    >> Option.map String.chars
    >> Option.defaultValue Seq.empty
    >> Seq.chunkBySize 2
    >> Seq.indexed
    >> Seq.choose (function
        | (id, [| size; space |]) ->
            { id = id
              size = size |> string |> int
              gap = space |> string |> int }
            |> Some
        | (id, [| size |]) ->
            { id = id
              size = size |> string |> int
              gap = 0 }
            |> Some
        | _ -> None)

let toMap =
    let fold (i, map) { id = id; size = size; gap = gap } =
        let fold map (j, id) = Map.add (i + j) id map

        (i + size + gap,
         id
         |> Seq.replicate size
         |> Seq.indexed
         |> Seq.fold fold map)

    Seq.fold fold (0, Map.empty) >> snd

let checksum =
    let fold sum index id = sum + (int64 index) * (int64 id)
    Map.fold fold 0L

let one: string seq -> int =

    let nextGap lastGapIndex lastBlock disk =
        seq {
            for i in lastGapIndex + 1 .. lastBlock do
                if not (Map.containsKey i disk) then
                    yield i
        }
        |> Seq.tryHead

    let compact =
        let rec compact' lastGapIndex disk =
            let (srcKey, src) = Map.maxKeyValue disk

            disk
            |> nextGap lastGapIndex srcKey
            |> function
                | Some destKey ->
                    disk
                    |> Map.remove srcKey
                    |> Map.add destKey src
                    |> compact' destKey
                | None -> disk

        compact' 0

    parse
    >> toMap
    >> compact
    >> checksum
    >> printfn "%A"
    >> (fun _ -> 0)

module Map =
    let tryMaxKeyValue map =
        if Map.isEmpty map then
            None
        else
            map |> Map.maxKeyValue |> Some

let two (lines: string seq) : int =
    let firstFit size disk =
        let occupied = disk |> Map.keys |> Set.ofSeq

        occupied
        |> Set.difference ({ 0 .. Set.maxElement occupied } |> Set.ofSeq)
        |> Seq.windowed size
        |> Seq.tryFind (fun window ->
            { Array.head window .. Array.last window }
            |> Seq.forall2 (=) window)
        |> Option.map Array.head

    let move start file disk =
        let fold disk i = Map.add i file.id disk

        { start .. start + file.size - 1 }
        |> Seq.fold fold (Map.filter (fun _ id -> id <> file.id) disk)

    let compact file disk =
        { 0 .. (disk |> Map.maxKeyValue |> fst) }
        |> Seq.iter (fun i ->
            disk
            |> Map.tryFind i
            |> function
                | Some id -> printf "%d" id
                | None -> printf ".")
        printf "\n"

        disk
        |> firstFit file.size
        |> function
            | Some start -> move start file disk
            | None -> disk

    let disk = parse lines

    disk
    |> toMap
    |> Seq.foldBack compact disk
    |> checksum
    |> printfn "%A"

    // disc |> compact
    0
