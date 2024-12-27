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

    let advanceTo
        (s: float)
        ({ position = (px, py)
           velocity = (vx, vy) })
        =
        let x = ((float px) + (s * (float vx))) % (float w)
        let x = if x < 0 then x + (float w) else x
        let y = ((float py) + (s * (float vy))) % (float h)
        let y = if y < 0 then y + (float h) else y
        x, y


type State =
    { robots: Robot seq
      startedAt: float option
      now: float
      playing: bool }


[<ReactComponent>]
let Main () =
    let (state, setState) =
        React.useState (
            { robots = Seq.empty
              startedAt = None
              playing = false
              now = 0.0 }
        )

    let changeFile (file: Types.File) =
        file
            .text()
            .``then`` (fun text -> setState ({ state with robots = Fourteen.parse text }))
        |> ignore

    let changeFrame (n: float) =
        setState (
            { state with
                startedAt = Some (n * -1000.0)
                now = 0.0 }
        )

    let frameRef = React.useRef None

    let elapsed =
        match state.startedAt with
        | Some startedAt -> (state.now - startedAt) / 1000.0
        | None -> 0.0

    React.useEffect (fun () ->
        match state with
        | { playing = true; startedAt = None } ->
            frameRef.current <-
                Some
                <| window.requestAnimationFrame (fun now -> setState { state with startedAt = Some now })
        | { playing = true } ->
            frameRef.current <-
                Some
                <| window.requestAnimationFrame (fun now -> setState { state with now = now })
        | { playing = false } when frameRef.current <> None -> Option.iter window.cancelAnimationFrame frameRef.current
        | _ -> ())

    let percent n d = length.percent ((n * 100.0) / float d)

    Html.div [ prop.style [ style.display.flex
                            style.flexDirection.column ]
               prop.children [ Html.div [ prop.style [ style.position.relative
                                                       style.height (length.vh 95)
                                                       style.backgroundColor.black
                                                       style.width (length.vw 100) ]
                                          prop.children [ for i, (x, y) in
                                                              state.robots
                                                              |> Seq.map (Fourteen.advanceTo elapsed)
                                                              |> Seq.indexed ->
                                                              Html.div [ prop.key i
                                                                         prop.style [ style.position.absolute
                                                                                      style.height (percent 1 h)
                                                                                      style.width (percent 1 w)
                                                                                      style.backgroundColor.white
                                                                                      style.top (percent y w)
                                                                                      style.left (percent x h) ] ] ] ]
                               Html.div [ Html.button [ prop.children [ Html.text "⏯️" ]
                                                        prop.onClick (fun _ ->
                                                            setState ({ state with playing = not state.playing })) ]
                                          Html.input [ prop.type'.number
                                                       prop.value (round elapsed)
                                                       prop.onChange changeFrame ]
                                          Html.input [ prop.type' "file"
                                                       prop.id "file"
                                                       prop.onChange changeFile ] ] ] ]


ReactDOM
    .createRoot(document.getElementById "root")
    .render (Main())
