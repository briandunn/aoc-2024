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

let one: string seq -> int =
    let checksum =
        let fold sum index id = sum + (int64 index) * (int64 id)
        Map.fold fold 0L

    let toMap =
        let fold (i, map) { id = id; size = size; gap = gap } =
            let fold map (j, id) = Map.add (i + j) id map

            (i + size + gap, id |> Seq.replicate size |> Seq.indexed |> Seq.fold fold map)

        Seq.fold fold (0, Map.empty) >> snd

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
                | Some destKey -> disk |> Map.remove srcKey |> Map.add destKey src |> compact' destKey
                | None -> disk

        compact' 0

    parse >> toMap >> compact >> checksum >> printfn "%A" >> (fun _ -> 0)


module Two =
    type File = { id: int; size: int; sector: int }

    type Disk =
        { files: File list
          gaps: Map<int, int> }

    let two: string seq -> int =
        let toDisk =
            let fold (i, disk) =
                function
                | { id = id; size = size; gap = gap } when gap = 0 ->
                    i + size,
                    { disk with
                        files = { id = id; size = size; sector = i } :: disk.files }
                | { id = id; size = size; gap = gap } ->
                    i + size + gap,
                    { files = { id = id; size = size; sector = i } :: disk.files
                      gaps = Map.add (i + size) gap disk.gaps }

            Seq.fold fold (0, { files = []; gaps = Map.empty }) >> snd

        let checksum: File list -> int64 =
            let fold
                sum
                { id = id
                  size = size
                  sector = sector }
                =
                sum
                + (seq { int64 sector .. (int64 sector + int64 size - 1L) }
                   |> Seq.map ((*) (int64 id))
                   |> Seq.sum)

            List.fold fold 0L

        let firstGap { size = size; sector = limit } gaps =
            gaps
            |> Map.toSeq
            |> Seq.takeWhile (fun (k, _) -> k < limit)
            |> Seq.choose (function
                | (sector, gap) when gap > size ->
                    Some(gaps |> Map.add (sector + size) (gap - size) |> Map.remove sector, sector)
                | (sector, gap) when gap = size -> Some(Map.remove sector gaps, sector)
                | _ -> None)
            |> Seq.tryHead

        let compact disk =
            let compact' (gaps, placed) file =
                gaps
                |> firstGap file
                |> function
                    | Some(gaps, destSector) -> (gaps, { file with sector = destSector } :: placed)
                    | None -> (gaps, file :: placed)

            List.fold compact' (disk.gaps, []) disk.files |> snd

        parse
        >> toDisk
        >> compact
        >> checksum
        >> (fun x ->
            printfn "%d" x
            0)

let two = Two.two
