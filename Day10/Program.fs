open System
open System.IO
open FParsec

let token p = p .>> spaces

let number  = token pint32
let comma   = token (pstring ",")
let lparen  = token (pchar '(')
let rparen  = token (pchar ')')
let lbrack  = token (pchar '[')
let rbrack  = token (pchar ']')
let lbrace  = token (pchar '{')
let rbrace  = token (pchar '}')

let lights = between lbrack rbrack (many (anyOf ['.'; '#']))
let circuit = between lparen rparen (sepBy1 number comma)
let circuits = many1 circuit
let joltage = between lbrace rbrace (sepBy1 number comma)
let machine = lights >>. circuits >>. joltage .>> eof

let parseMachine s =
    match run machine s with
    | Success (v,_,_) -> v
    | Failure (error, _, _) ->
        failwithf "Parsing error: %s" error

let result1 input =
    File.ReadLines(input)
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.map parseMachine

printfn "Part 1 Example: %A" (result1 "example.txt")
//printfn "Part 1 Result: %A" (result1 "input.txt")
