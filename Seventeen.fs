module Seventeen

type Register =
    | A
    | B
    | C

type Operation =
    | adv = 0
    | bxl = 1
    | bst = 2
    | jnz = 3
    | bxc = 4
    | out = 5
    | bdv = 6
    | cdv = 7

type Combo =
    | Literal of int
    | Register of Register

type Instruction =
    | Adv of Combo
    | Bdv of Combo
    | Bst of Combo
    | Cdv of Combo
    | Out of Combo
    | Bxl of int
    | Jnz of int
    | Bxc

type Program =
    { registers: Map<Register, int>
      instructions: int array }

type State =
    { registers: Map<Register, int>
      output: int list
      instructionPointer: int }

module Instruction =
    let parse operation operand =
        let parseCombo =
            function
            | 4 -> Register A
            | 5 -> Register B
            | 6 -> Register C
            | v -> v |> Literal

        match enum<Operation> operation with
        | Operation.adv -> Adv(parseCombo operand)
        | Operation.bxl -> Bxl operand
        | Operation.bst -> Bst(parseCombo operand)
        | Operation.jnz -> Jnz operand
        | Operation.bxc -> Bxc
        | Operation.out -> Out(parseCombo operand)
        | Operation.bdv -> Bdv(parseCombo operand)
        | Operation.cdv -> Cdv(parseCombo operand)
        | _ -> failwith "Invalid operation"
        |> fun o ->
            printfn "%A" o
            o

    let execute state =
        let combo =
            function
            | Register r -> Map.find r state.registers
            | Literal v -> v

        let inc state =
            { state with
                instructionPointer = state.instructionPointer + 2 }

        let dv o dest =
            let numerator = Map.find A state.registers
            let denominator = o |> combo |> pown 2
            let result = numerator / denominator

            { state with
                registers = Map.add dest result state.registers }
            |> inc

        function
        | Adv o -> dv o A
        | Bdv o -> dv o B
        | Cdv o -> dv o C
        | Bxl o ->
            { state with
                registers = Map.change B (Option.map ((^^^) o)) state.registers }
            |> inc

        | Bst o ->
            { state with
                registers = Map.add B ((combo o) % 8) state.registers }
            |> inc

        | Jnz _ when Map.find A state.registers = 0 -> inc state
        | Jnz o -> { state with instructionPointer = o }

        | Bxc ->
            { state with
                registers = Map.change B (Option.map ((^^^) (Map.find C state.registers))) state.registers }
            |> inc

        | Out o ->
            { state with
                output = (combo o) % 8 :: state.output }
            |> inc

module Regex =
    let matches string regex =
        let regex = System.Text.RegularExpressions.Regex(regex)

        [ for m in regex.Matches(string) do
              for s in (Seq.skip 1 m.Groups) do
                  yield s.Value ]

let parse lines =
    let parseInstructions =
        let map line = Regex.matches line "([0-7])"

        Seq.map map >> Seq.concat >> Seq.map int >> Array.ofSeq

    let parseRegisters =

        let fold acc line =
            match Regex.matches line "([A-C]): (\d+)" with
            | [ "A"; v ] -> v |> int |> Map.add A
            | [ "B"; v ] -> v |> int |> Map.add B
            | [ "C"; v ] -> v |> int |> Map.add C
            | _ -> id
            <| acc

        Seq.fold fold Map.empty

    { registers = lines |> Seq.takeWhile ((<>) "") |> parseRegisters
      instructions = lines |> parseInstructions }

let one lines =
    let program = lines |> parse
    printfn "%A" program

    program.instructions
    |> Seq.chunkBySize 2
    |> Seq.iter (function
        | [| a; b |] -> Instruction.parse a b |> printfn "%A"
        | _ -> ())

    let rec run state =
        printfn "%A" state

        [ state.instructionPointer; state.instructionPointer + 1 ]
        |> List.choose (fun i -> program.instructions |> Array.tryItem i)
        |> function
            | [ operation; operand ] -> run (operand |> Instruction.parse operation |> Instruction.execute state)
            | _ -> List.rev state.output

    run
        { instructionPointer = 0
          registers = program.registers
          output = [] }

    |> Seq.map string
    |> String.concat ","
    |> printfn "%s"

    0

let two lines = 0
