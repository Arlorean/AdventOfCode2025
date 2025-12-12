open System
open System.IO
open FParsec
open Microsoft.Z3
open Microsoft.Z3.Int
open Microsoft.Z3.Bool

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

let distributeByCount count elements =
    let rec distribute remaining values acc =
        match values with
        | [] -> 
            if remaining = 0 then [List.rev acc]
            else []
        | v :: rest ->
            [0 .. remaining]
            |> List.collect (fun count ->
                let newAcc = if count > 0 then (v, count) :: acc else acc
                distribute (remaining - count) rest newAcc
            )
    
    distribute count elements []

let choiceCombinations (choices: 'a list array) =
    let rec loop idx acc = seq {
        if idx = choices.Length then
            // we've picked one from each slot
            yield List.rev acc
        else
            // try every option in this slot
            for choice in choices[idx] do
                yield! loop (idx + 1) (choice :: acc)
    }
    loop 0 []


let buttonIndicesAffectingJoltage machine joltageIndex  =
    machine.Buttons
    |> Array.indexed
    |> Array.filter (Array.exists ((=) joltageIndex) << snd)
    |> Array.map fst

let joltageButtonCombinations machine joltageIndex =
    buttonIndicesAffectingJoltage machine joltageIndex
    |> Array.toList
    |> distributeByCount (machine.Joltages[joltageIndex]) 

let isValidSolution machine (buttonPresses : (int*int) list)  =
    let hasConflictingPressCounts = 
        buttonPresses
        |> List.groupBy fst
        |> List.map snd
        |> List.map List.length
        |> List.exists ((>) 1)

    if hasConflictingPressCounts then
        false
    else
        let mutable counts = Array.zeroCreate machine.Joltages.Length
        buttonPresses
            |> List.iter (fun (buttonIndex, pressCount) ->
                let buttonJoltages =
                    machine.Buttons[buttonIndex]
                buttonJoltages
                |> Array.iter (fun joltageIndex ->
                    counts[joltageIndex] <- counts[joltageIndex] + pressCount
                )
            )
        Array.forall2 (=) counts machine.Joltages

let machineResult2 machine =
    [|0..machine.Joltages.Length - 1|]
    |> Array.map (joltageButtonCombinations machine)
    |> choiceCombinations
    |> Seq.map List.concat
    |> Seq.map List.distinct
    |> Seq.filter (isValidSolution machine)
    |> Seq.map (List.sumBy snd)
    |> Seq.min

let result2 input = 
    parseMachinesFile input
    |> Seq.map machineResult2

//printfn "Part 2 Example: %A" (result2 "example.txt")
//printfn "Part 2 Result: %A" (result2 "input.txt")

// 3 = e + f
// 5 = b + f
// 4 = c + d + e
// 7 = a + b + d

let test=
    let a = Int("a")
    let b = Int("b")
    let c = Int("c")
    let d = Int("d")
    let e = Int("e")
    let f = Int("f")
    let sum = Int("sum")

    let constraints = [|
        a >=. 0I
        b >=. 0I
        c >=. 0I
        d >=. 0I
        e >=. 0I
        f >=. 0I

        e + f =. 3I
        b + f =. 5I
        c + d + e =. 4I
        a + b + d =. 7I

        sum =. a + b + c + d + e + f
    |]
    let constraintExprs = 
        constraints
        |> Array.map (fun c -> c.Expr :?> BoolExpr)

    let optimizer = Gs.context().MkOptimize()
    optimizer.Add constraintExprs
    optimizer.MkMinimize(sum.Expr :?> ArithExpr) |> ignore

    match optimizer.Check() with
    | Status.SATISFIABLE ->
        let model = optimizer.Model
        printfn "Z3 Success: %O" (model.Evaluate sum.Expr)
    | failure ->
        printfn "Z3 Failure: %O" failure

test |> ignore


let toArithExpr (expr:Expr) = expr :?> ArithExpr

let solve machine =
    let context = new Context()
    let optimizer = context.MkOptimize()

    let mkButtonVar buttonIndex =
        let var = context.MkInt($"button_{buttonIndex}")
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

let resultz input = 
    parseMachinesFile input
    |> Seq.map solve

resultz "example.txt"
    |> Seq.iter (printfn "%A")