open System
open System.IO
open FParsec
open Microsoft.Z3

type Machine = {
    Lights: int
    Buttons: int [][]
    Joltages: int []
} with
    static member Create lights  buttons joltages=
        { Lights = lights; Buttons = buttons; Joltages = joltages }


let inline flip f x y = f y x

let bitIndicesToInt =
    Seq.map ((<<<) 1) 
    >> Seq.fold (+) 0

let bitsToInt =
    Seq.mapi (flip (<<<))
    >> Seq.fold (+) 0

let lightToBit = function
    | '.' -> 0
    | '#' -> 1
    | _   -> failwith "invalid light state"


let token p = p .>> spaces

let number  = token pint32
let comma   = token (pstring ",")
let lparen  = token (pchar '(')
let rparen  = token (pchar ')')
let lbrack  = token (pchar '[')
let rbrack  = token (pchar ']')
let lbrace  = token (pchar '{')
let rbrace  = token (pchar '}')

let light = anyOf ['.'; '#'] |>> lightToBit
let lights = between lbrack rbrack (many light) |>> bitsToInt
let button = between lparen rparen (sepBy1 number comma) |>> List.toArray
let buttons = many1 button |>> List.toArray
let joltage = between lbrace rbrace (sepBy1 number comma) |>> List.toArray
let machine = pipe3 lights buttons joltage Machine.Create .>> eof

let parseMachine s =
    match run machine s with
    | Success (machine,_,_) -> machine
    | Failure (error,_,_) -> failwith error

let parseMachinesFile input =
    File.ReadLines(input)
    |> Seq.filter (not << String.IsNullOrWhiteSpace)
    |> Seq.map parseMachine


let combinations elements =
    let n = Array.length elements
    let rec choose k start = seq {
        if k = 0 then yield []
        elif start < n then
            for i in start .. n - 1 do
                for tail in choose (k - 1) (i + 1) do
                    yield elements[i] :: tail
    }
    seq {
        for k in 0 .. n do
            yield! choose k 0
    }


let buttonCombinations machine =
    machine.Buttons
    |> combinations

let pressButton lightState buttonLightToggles =
    lightState ^^^ (bitIndicesToInt buttonLightToggles)

let pressButtons = List.fold pressButton 0

let machineResult1 machine =
    buttonCombinations machine
    |> Seq.find (pressButtons >> (=) machine.Lights)

let result1 input =
    parseMachinesFile input
    |> Seq.map machineResult1
    |> Seq.map List.length
    |> Seq.fold (+) 0


printfn "Part 1 Example: %A" (result1 "example.txt")
printfn "Part 1 Result: %A" (result1 "input.txt")


let buttonIndicesAffectingJoltage machine joltageIndex  =
    machine.Buttons
    |> Array.indexed
    |> Array.filter (Array.exists ((=) joltageIndex) << snd)
    |> Array.map fst

let toArithExpr (expr:Expr) = expr :?> ArithExpr

let solve machine =
    let context = new Context()
    let optimizer = context.MkOptimize()

    let mkButtonVar buttonIndex =
        let var = context.MkIntConst($"button_{buttonIndex}")
        context.MkGe(var, context.MkInt(0)) |> optimizer.Add
        var

    let buttonVars =
        [0..machine.Buttons.Length - 1]
        |> List.map mkButtonVar
        |> List.toArray

    let mkJoltageExpr joltageIndex =
        let sumExpr =
            buttonIndicesAffectingJoltage machine joltageIndex
            |> Array.map (Array.get buttonVars)
            |> Array.map toArithExpr
            |> context.MkAdd
        let joltageValueExpr = context.MkInt(machine.Joltages[joltageIndex])
        let joltageExpr = context.MkEq(sumExpr, joltageValueExpr)
        optimizer.Add(joltageExpr) |> ignore
        joltageExpr

    let joltageExprs =
        [0..machine.Joltages.Length - 1]
        |> List.map mkJoltageExpr
        |> List.toArray

    let totalPressesExpr =
        buttonVars
        |> Array.map toArithExpr
        |> context.MkAdd

    optimizer.Add(joltageExprs) |> ignore
    optimizer.MkMinimize(totalPressesExpr) |> ignore

    match optimizer.Check() with
    | Status.SATISFIABLE ->
        let model = optimizer.Model
        let totalPresses = model.Evaluate(totalPressesExpr) :?> IntNum
        Some (int totalPresses.Int)
    | _ ->
        None

let result2 input = 
    parseMachinesFile input
    |> Seq.map solve
    |> Seq.choose id
    |> Seq.sum

printfn "Part 2 Example: %A" (result2 "example.txt")
printfn "Part 2 Result: %A" (result2 "input.txt")
