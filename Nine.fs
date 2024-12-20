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
    type File = { id: int; size: int }

    type Sector =
        | Gap of int
        | File of File

    let two: string seq -> int =
        let toMap =
            let fold (i, map) =
                function
                | { id = id; size = size; gap = gap } when gap = 0 ->
                    i + size, map |> Map.add i (File { id = id; size = size })
                | { id = id; size = size; gap = gap } ->
                    i + size + gap, map |> Map.add i (File { id = id; size = size }) |> Map.add (i + size) (Gap gap)

            Seq.fold fold (0, Map.empty) >> snd

        let checksum: Map<int, Sector> -> int64 =
            let fold sum sector =
                function
                | File { id = id; size = size } ->
                    sum
                    + (seq { int64 sector .. (int64 sector + int64 size - 1L) }
                       |> Seq.map ((*) (int64 id))
                       |> Seq.sum)
                | _ -> sum

            Map.fold fold 0L

        let firstGap limit { size = size } =
            Map.toSeq
            >> Seq.takeWhile (fun (k, _) -> k < limit)
            >> Seq.choose (function
                | (sector, Gap gap) when gap > size -> Some(sector, Some(sector + size, Gap(gap - size)))
                | (sector, Gap gap) when gap = size -> Some(sector, None)
                | _ -> None)
            >> Seq.tryHead

        let lastFile stuckCount =
            Map.toSeq
            >> Seq.rev
            >> Seq.choose (function
                | (sector, File file) -> Some(sector, file)
                | _ -> None)
            >> Seq.skip stuckCount
            >> Seq.tryHead

        let compact disk =
            let rec compact' stuckCount disk =
                match lastFile stuckCount disk with
                | None -> disk
                | Some(srcSector, file) ->
                    disk
                    |> firstGap srcSector file
                    |> function
                        | Some(destSector, Some(gapSector, newGap)) ->
                            disk
                            |> Map.remove srcSector
                            |> Map.add destSector (File file)
                            |> Map.add gapSector newGap
                            |> compact' stuckCount
                        | Some(destSector, None) ->
                            disk
                            |> Map.remove srcSector
                            |> Map.add destSector (File file)
                            |> compact' stuckCount
                        | None -> compact' (stuckCount + 1) disk

            compact' 0 disk

        parse
        >> toMap
        >> compact
        >> checksum
        >> (fun x ->
            printfn "%d" x
            0)

module Two' =
    type File = { id: int; size: int; sector: int }

    type Disk =
        { files: File list
          gaps: Map<int, int> }

    let printGaps gapMap =
        let lastSector =
            if Map.isEmpty gapMap then 0
            else
                gapMap
                |> Map.maxKeyValue
                |> function
                    | (sector, _) -> sector

        let iter sector =
            match Map.tryFind sector gapMap with
            | Some size -> '.' |> Seq.replicate size |> Seq.iter (printf "%c")
            | None -> printf " "

        seq { 0..lastSector } |> Seq.iter iter
        printfn ""
    let printFiles files =
        let fold map file = Map.add file.sector file map
        let fileMap = List.fold fold Map.empty files

        let lastSector =
            if Map.isEmpty fileMap then 0
            else
                fileMap
                |> Map.maxKeyValue
                |> function
                    | (sector, _) -> sector

        let iter sector =
            match Map.tryFind sector fileMap with
            | Some { id = id; size = size } -> id |> Seq.replicate size |> Seq.iter (printf "%d")
            | None -> printf " "

        seq { 0..lastSector } |> Seq.iter iter
        printfn ""

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

        let firstGap { size = size; sector = limit } =
            Map.toSeq
            >> Seq.takeWhile (fun (k, _) -> k < limit)
            >> Seq.choose (function
                | (sector, gap) when gap > size -> Some(sector, Some(sector + size, (gap - size)))
                | (sector, gap) when gap = size -> Some(sector, None)
                | _ -> None)
            >> Seq.tryHead

        let compact disk =
            let compact' (gaps, placed) file =
                gaps
                |> firstGap file
                |> function
                    | Some(destSector, Some(gapSector, newGap)) ->
                        (gaps |> Map.add gapSector newGap |> Map.remove destSector, { file with sector = destSector } :: placed)
                    | Some(destSector, None) -> (Map.remove destSector gaps, { file with sector = destSector } :: placed)
                    | None -> (gaps, file :: placed)

            List.fold compact' (disk.gaps, []) disk.files |> snd

        parse
        >> toDisk
        >> compact
        >> checksum
        >> (fun x ->
            printfn "%d" x
            0)

let two = Two'.two