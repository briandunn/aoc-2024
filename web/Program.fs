open Browser
open Feliz

type Robot =
    { position: int * int
      velocity: int * int }

let (w, h) = (101, 103)

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

    //     let (w, h) = (101, 103)

    let advance
        ({ position = (px, py)
           velocity = (vx, vy) } as robot)
        =
        let x = (px + vx) % w
        let x = if x < 0 then x + w else x
        let y = (py + vy) % h
        let y = if y < 0 then y + h else y
        { robot with position = x, y }

    let advanceTo (s: int)
        ({ position = (px, py)
           velocity = (vx, vy) } as robot)
        =
        let x = (px + (s * vx)) % w
        let x = if x < 0 then x + w else x
        let y = (py + (s * vy)) % h
        let y = if y < 0 then y + h else y
        { robot with position = x, y }

//     let draw (context: Types.CanvasRenderingContext2D) robots =
//         context.clearRect (0.0, 0.0, context.canvas.width, context.canvas.height)
//         let scaleW = context.canvas.width / (float w)
//         let scaleH = context.canvas.height / (float h)

//         Seq.iter
//             (fun { position = (x, y) } -> context.fillRect (float x * scaleW, float y * scaleH, scaleW, scaleH))
//             robots

//     let two (stateRef: State ref) =
//         let { context = context; robots = robots; playing = playing} = stateRef.Value
//         let requestFrame f = stateRef.Value <- {stateRef.Value with frame = Some(window.requestAnimationFrame(f))}
//         let rec tick prev robots current =
//             if (current - prev) > 30.0 then
//                 draw context robots

//                 stateRef.Value <- {stateRef.Value with robots = robots |> Seq.map advance}

//                 stateRef.Value.robots
//                 |> tick current
//                 |> requestFrame
//             else
//                 robots
//                 |> tick prev
//                 |> requestFrame
//                 |> ignore

//         if playing then
//           robots
//           |> tick 0.0
//           |> requestFrame
//           |> ignore
//         else
//           match stateRef.Value.frame with
//           | Some frame -> window.cancelAnimationFrame frame
//           | None -> ()

// let canvas = document.getElementById "canvas" :?> Types.HTMLCanvasElement
// let context = canvas.getContext "2d" :?> Types.CanvasRenderingContext2D
// let file = document.getElementById ("file") :?> Types.HTMLInputElement

// let state = ref { robots = Seq.empty; playing = false; context = context; frame = None }

// document
//     .getElementById("play")
//     .addEventListener (
//         "click",
//         (fun _ ->
//             state.Value <- { state.Value with playing = not state.Value.playing }
//             Fourteen.two state)
//     )

type State =
    { robots: Robot seq
      playing: bool
      frame: int }


[<ReactComponent>]
let Main () =
    let (state, setState) =
        React.useState (
            { robots = Seq.empty
              playing = false
              frame = 0 }
        )

    let changeFile (file: Types.File) =
        file
            .text()
            .``then`` (fun text -> setState ({ state with robots = Fourteen.parse text }))
        |> ignore

    let changeFrame (n: int) =
        setState ({ state with frame = n })

    React.useEffect (fun () ->
        if state.playing then
            window.requestAnimationFrame (fun _ -> changeFrame (state.frame + 1)) |> ignore
    )

    let percent n d =
        length.percent ((float n * 100.0) / float d)

    Html.div [ Html.input [ prop.type' "file"
                            prop.id "file"
                            prop.onChange changeFile ]
               Html.div [ prop.style [ style.position.relative
                                       style.height 400
                                       style.width 600 ]
                          prop.children [ for i, { position = (x, y) } in state.robots |> Seq.map (Fourteen.advanceTo state.frame) |> Seq.indexed ->
                                              Html.div [ prop.key i
                                                         prop.style [ style.position.absolute
                                                                      style.height (percent 1 h)
                                                                      style.width (percent 1 w)
                                                                      style.backgroundColor color.black
                                                                      style.top (percent y w)
                                                                      style.left (percent x h) ] ] ] ]
               Html.button [ prop.children [ Html.text "⏯️" ]
                             prop.onClick (fun _ -> setState ({ state with playing = not state.playing })) ]
               Html.input [ prop.type'.number
                            prop.value state.frame
                            prop.onChange changeFrame ] ]


ReactDOM
    .createRoot(document.getElementById "root")
    .render (Main())
