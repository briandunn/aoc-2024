module Thirteen

type Pt = decimal * decimal
type Machine = { a: Pt; b: Pt; prize: Pt }

let parse: string seq -> Machine seq =
    let regex = System.Text.RegularExpressions.Regex("(\d+)")

    let parsePoint line =
        match regex.Matches(line) |> Seq.map (fun m -> decimal m.Value) |> Seq.toArray with
        | [| x; y |] -> Some(x, y)
        | _ -> None

    let parseMachine =
        Array.choose parsePoint
        >> function
            | [| a; b; prize |] -> Some({ a = a; b = b; prize = prize })
            | _ -> None

    Seq.chunkBySize 4 >> Seq.choose (Array.take 3 >> parseMachine)


let times (x, y) n = (x * n, y * n)
let plus (xa, ya) (xb, yb) = (xa + xb, ya + yb)

let one: string seq -> int =

    let permutations =
        seq {
            for a in 0M .. 100M do
                for b in 0M .. 100M do
                    yield a, b
        }


    let solve ({ a = a; b = b; prize = prize } as machine) =
        permutations
        |> Seq.filter (fun (a', b') -> plus (times a a') (times b b') = prize)
        |> Seq.tryHead
        |> function
            | None -> None
            | Some x ->
                // printfn "%A" machine
                printfn "%A" x
                Some x
        |> Option.map (fun (a', b') -> (a' * 3M + b'))

    // 29334 - too low
    parse
    // >> Seq.skip 10
    // >> Seq.take 10
    >> Seq.choose solve
    >> Seq.sum
    >> (fun x ->
        printfn "%A" x
        0)

let two: string seq -> int =
    let adjustPrize machine =
        { machine with
            prize = plus machine.prize (10000000000000M, 10000000000000M) }

    let xCross (aX, aY) (bX, bY) (prizeX, prizeY) =
        // if a goes through prize, at what x do a and b cross?
        let mA = aY / aX
        let mB = bY / bX
        round ((prizeX * mA - prizeY) / (mA - mB))

    let solve
        { a = (aX, _ as a)
          b = (bX, _ as b)
          prize = ((prizeX, _) as prize) }
        =
        let x = xCross a b prize

        let remainingX = prizeX - x

        if x % bX <> 0M || remainingX % aX <> 0M then
            None
        else
            Some(remainingX / aX, x / bX)

    parse
    >> Seq.map adjustPrize
    >> Seq.choose solve
    >> Seq.map (fun (a', b') -> (a' * 3M + b'))
    >> Seq.sum
    >> (fun sum ->
        printfn "%M" sum
        0)
