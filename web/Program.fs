open Browser

module Fourteen = 
  type Robot =
      { position: int * int
        velocity: int * int }

  let regex = System.Text.RegularExpressions.Regex("-?\d+")

  let parse (file: string) =
      let parse line =
          match regex.Matches(line) |> Seq.map (fun m -> int m.Value) |> Seq.toArray with
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
    context.clearRect(0.0, 0.0, context.canvas.width, context.canvas.height)
    let scaleW = context.canvas.width / (float w)
    let scaleH = context.canvas.height / (float h)

    Seq.iter (fun { position = (x, y) } ->
        context.fillRect(float x * scaleW, float y * scaleH, scaleW, scaleH)
    ) robots

  let two (context: Types.CanvasRenderingContext2D) input =
      let rec tick prev robots current =
          if (current - prev) > 30.0 then
            draw context robots
            robots |> Seq.map advance |> tick current |> window.requestAnimationFrame |> ignore
          else
            robots |> tick prev |> window.requestAnimationFrame |> ignore
      window.requestAnimationFrame (input |> parse |> tick 0.0) |> ignore

let canvas = document.createElement "canvas" :?> Types.HTMLCanvasElement
canvas.width <- 600
canvas.height <- 600
document.body.appendChild canvas |> ignore
let context = canvas.getContext "2d" :?> Types.CanvasRenderingContext2D
let file = document.createElement("input") :?> Types.HTMLInputElement
file.``type`` <- "file"
file.addEventListener("change", (fun e -> 
    let file = (e.target :?> Types.HTMLInputElement).files.item(0)

    file.text().``then``(Fourteen.two context) |> ignore
))

document.body.appendChild file |> ignore