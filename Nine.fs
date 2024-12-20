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

type File' = { id: int; size: int }

type Sector =
    | Gap of int
    | File of File'

let two: string seq -> int =
    let print disk =
        let iter (_, sectorType) =
            match sectorType with
            | Gap gap -> '.' |> Seq.replicate gap |> Seq.iter (printf "%c")
            | File { id = id; size = size } -> id |> Seq.replicate size |> Seq.iter (printf "%d")

        disk |> Map.toSeq |> Seq.iter iter
        printf "\n"

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
            print disk

            match lastFile stuckCount disk with
            | None -> disk
            | Some(srcSector, file) ->
                printfn "id: %d" file.id

                disk
                |> firstGap srcSector file
                |> function
                    | Some(destSector, Some(gapSector, newGap)) ->
                        disk
                        |> Map.remove srcSector
                        |> Map.add srcSector (Gap file.size)
                        |> Map.add destSector (File file)
                        |> Map.add gapSector newGap
                        |> compact' stuckCount
                    | Some(destSector, None) ->
                        disk
                        |> Map.remove srcSector
                        |> Map.add srcSector (Gap file.size)
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
