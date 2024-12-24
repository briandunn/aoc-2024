module Thirteen

type Pt = int64 * int64
type Machine = { a: Pt; b: Pt; prize: Pt }

let parse: string seq -> Machine seq =
    let regex = System.Text.RegularExpressions.Regex("(\d+)")

    let parsePoint line =
        match regex.Matches(line) |> Seq.map (fun m -> int64 m.Value) |> Seq.toArray with
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
            for a in 0L .. 100L do
                for b in 0L .. 100L do
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
        |> Option.map (fun (a', b') -> (a' * 3L + b'))

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
            prize = plus machine.prize (10000000000000L, 10000000000000L) }

    let xCross (aX, aY) (bX, bY) (prizeX, prizeY) =
        // if a goes through prize, at what x do a and b cross?
        let mA = (float aY) / (double aX)
        let mB = (float bY) / (double bX)
        let yInterceptA = float prizeY - mA * double prizeX

        -yInterceptA / (mA - mB)

    let solve
        ({ a = (_, aY as a)
           b = (_, bY as b)
           prize = ((_, prizeY) as prize) } as machine)
        =
        let x = xCross a b prize
        let bPresses = x / (b |> fst |> float)

        if abs (bPresses - round bPresses) > 0.00001 then
            None
        else
            let bPresses = round bPresses
            let crossY = bPresses * (float bY)
            let diffY = (float prizeY) - crossY
            let aPresses = round (diffY / (float aY))

            // printfn "%A" machine
            printfn "%A" (int64 aPresses, int64 bPresses)
            Some(int64 aPresses, int64 bPresses)

        |> Option.map (fun (a', b') -> (a' * 3L + b'))

    parse
    // >> Seq.map adjustPrize
    // >> Seq.skip 10
    // >> Seq.take 10
    >> Seq.choose solve
    >> Seq.sum
    >> (fun sum ->
        printfn "%d" sum
        0)
