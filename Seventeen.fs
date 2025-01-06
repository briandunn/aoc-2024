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
    { registers: Map<Register, int64>
      raw: int seq
      instructions: Instruction array }

type State =
    { registers: Map<Register, int64>
      output: int64 option
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
            let denominator = o |> combo |> int |> pown 2L
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
                registers = Map.add B ((combo o) % 8L) state.registers }
            |> inc

        | Jnz _ when Map.find A state.registers = 0 -> inc state
        | Jnz o -> { state with instructionPointer = o }

        | Bxc ->
            { state with
                registers = Map.change B (Option.map ((^^^) (Map.find C state.registers))) state.registers }
            |> inc

        | Out o ->
            { state with
                output = Some((combo o) % 8L) }
            |> inc

module Regex =
    let matches string regex =
        let regex = System.Text.RegularExpressions.Regex(regex)

        [ for m in regex.Matches(string) do
              for s in (Seq.skip 1 m.Groups) do
                  yield s.Value ]

let parse lines =
    let parseInstructions =
        Seq.chunkBySize 2
        >> Seq.choose (function
            | [| a; b |] -> b |> Instruction.parse a |> Some
            | _ -> None)
        >> Seq.toArray

    let parseRegisters =

        let fold acc line =
            let add k v = Map.add k (int64 v)

            match Regex.matches line "([A-C]): (\d+)" with
            | [ "A"; v ] -> add A v
            | [ "B"; v ] -> add B v
            | [ "C"; v ] -> add C v
            | _ -> id
            <| acc

        Seq.fold fold Map.empty

    let registers = lines |> Seq.takeWhile ((<>) "") |> parseRegisters

    let parseProgram =
        let map line = Regex.matches line "([0-7])"
        Seq.map map >> Seq.concat >> Seq.map int

    let raw = lines |> parseProgram |> Seq.cache

    { registers = registers
      raw = raw
      instructions = parseInstructions raw }

let run program =
    let rec run state =
        program.instructions
        |> Array.tryItem (state.instructionPointer / 2)
        |> function
            | Some instruction ->
                match Instruction.execute state instruction with
                | { output = Some o } as state -> Some(o, { state with output = None })
                | state -> run state
            | _ -> None

    { instructionPointer = 0
      registers = program.registers
      output = None }
    |> Seq.unfold run


let one lines =
    lines |> (parse >> run >> Seq.map string >> String.concat ",") |> printfn "%s"
    0

let two lines =
    let program = parse lines

    printfn "%A" program.registers
    program.instructions |> Seq.iter (printfn "%A")

    // values of A that will produce 16 outputs:
    let min = pown 8L 15
    let max = (pown 8L 16) - 1L

    // thats still 246 trillion.

    let raw = program.raw |> Seq.map int64

    let choose =
        let tryFind a =
            { program with
                registers = Map.add A a program.registers }
            |> run
            |> Seq.zip raw
            |> Seq.forall (fun (r, o) -> r = o)

        Array.Parallel.tryFind tryFind

    seq { min..max }
    |> Seq.windowed 4096
    |> Seq.choose choose
    |> Seq.head
    |> printfn "%A"

    0
