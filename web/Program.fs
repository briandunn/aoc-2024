open Browser

type Robot =
    { position: int * int
      velocity: int * int }

type State = { robots: Robot seq; playing: bool; context: Types.CanvasRenderingContext2D; frame: float option }

module Fourteen =
    let regex = System.Text.RegularExpressions.Regex("-?\d+")

    let parse (file: string) =
        let parse line =
            match regex.Matches(line)
                  |> Seq.map (fun m -> int m.Value)
                  |> Seq.toArray
                with
            | [| px; py; vx; vy |] ->
                Some(
                    { position = (px, py)
                      velocity = (vx, vy) }
                )
            | _ -> None

        file.Split('\n') |> Seq.choose parse |> Seq.cache

    let (w, h) = (101, 103)

    let advance
        ({ position = (px, py)
           velocity = (vx, vy) } as robot)
        =
        let x = (px + vx) % w
        let x = if x < 0 then x + w else x
        let y = (py + vy) % h
        let y = if y < 0 then y + h else y
        { robot with position = x, y }

    let draw (context: Types.CanvasRenderingContext2D) robots =
        context.clearRect (0.0, 0.0, context.canvas.width, context.canvas.height)
        let scaleW = context.canvas.width / (float w)
        let scaleH = context.canvas.height / (float h)

        Seq.iter
            (fun { position = (x, y) } -> context.fillRect (float x * scaleW, float y * scaleH, scaleW, scaleH))
            robots

    let two (stateRef: State ref) =
        let { context = context; robots = robots; playing = playing} = stateRef.Value
        let requestFrame f = stateRef.Value <- {stateRef.Value with frame = Some(window.requestAnimationFrame(f))}
        let rec tick prev robots current =
            if (current - prev) > 30.0 then
                draw context robots

                stateRef.Value <- {stateRef.Value with robots = robots |> Seq.map advance}

                stateRef.Value.robots
                |> tick current
                |> requestFrame
            else
                robots
                |> tick prev
                |> requestFrame
                |> ignore

        if playing then
          robots
          |> tick 0.0
          |> requestFrame
          |> ignore
        else
          match stateRef.Value.frame with
          | Some frame -> window.cancelAnimationFrame frame
          | None -> ()

let canvas = document.getElementById "canvas" :?> Types.HTMLCanvasElement
let context = canvas.getContext "2d" :?> Types.CanvasRenderingContext2D
let file = document.getElementById ("file") :?> Types.HTMLInputElement

let state = ref { robots = Seq.empty; playing = false; context = context; frame = None }

document
    .getElementById("play")
    .addEventListener (
        "click",
        (fun _ ->
            state.Value <- { state.Value with playing = not state.Value.playing }
            Fourteen.two state)
    )

file.addEventListener (
    "change",
    (fun e ->
        let file =
            (e.target :?> Types.HTMLInputElement)
                .files.item (0)

        file
            .text()
            .``then`` (fun text ->
                state.Value <- { state.Value with robots = Fourteen.parse text }
                Fourteen.two state)
        |> ignore)
)
