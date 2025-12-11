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

let result1 input =
    File.ReadLines(input)
    |> Seq.filter (not << String.IsNullOrWhiteSpace)
    |> Seq.map parseMachine

printfn "Part 1 Example: %A" (result1 "example.txt")
//printfn "Part 1 Result: %A" (result1 "input.txt")
