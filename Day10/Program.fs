open System
open System.IO
open FParsec

type Machine = {
    Lights: int
    Buttons: int list
    Joltage: int list
} with
    static member Create lights  buttons joltage=
        { Lights = lights; Buttons = buttons; Joltage = joltage }

let formatLights n lights =
    seq {
        yield "["
        for i in 0 .. n do
            if (lights &&& (1 <<< i)) <> 0 then
                yield "#"
            else
                yield "."
        yield "]"
    } |> String.Concat

let formatButton button =
    seq {
        yield "("
        yield String.Join(",",
            seq {
                for i: int32 in 0 .. 31 do
                    if (button &&& (1 <<< i)) <> 0 then
                        yield i.ToString()
            }
        )
        yield ")"
    } |> String.Concat

let formatButtons buttons =
    buttons
    |> List.map formatButton
    |> String.concat " "


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
let button = between lparen rparen (sepBy1 number comma) |>> bitIndicesToInt
let buttons = many1 button
let joltage = between lbrace rbrace (sepBy1 number comma)
let machine = pipe3 lights buttons joltage Machine.Create .>> eof

let parseMachine s =
    match run machine s with
    | Success (machine,_,_) -> machine
    | Failure (error,_,_) -> failwith error

let parseMachinesFile input =
    File.ReadLines(input)
    |> Seq.filter (not << String.IsNullOrWhiteSpace)
    |> Seq.map parseMachine


module List =
    let combinations xs =
        let n = List.length xs

        let rec choose k start = seq {
            if k = 0 then yield []
            elif start < n then
                for i in start .. n - 1 do
                    for tail in choose (k - 1) (i + 1) do
                        yield xs[i] :: tail
        }

        seq {
            for k in 0 .. n do
                yield! choose k 0
        }

let buttonCombinations machine =
    machine.Buttons
    |> List.combinations

let pressButton lightState buttonLightsToggle  =
    lightState ^^^ buttonLightsToggle 

let pressButtons = List.fold pressButton 0

let machineResult machine =
    buttonCombinations machine
    |> Seq.find (pressButtons >> (=) machine.Lights)

let result1 input =
    parseMachinesFile input
    |> Seq.map machineResult
    |> Seq.map List.length
    |> Seq.fold (+) 0


printfn "Part 1 Example: %A" (result1 "example.txt")
printfn "Part 1 Result: %A" (result1 "input.txt")

