open System
open System.IO
open FParsec

let parser = anyOf ['L'; 'R'] .>>. pint32 .>> eof

let parseTurn s =
    match run parser s with
    | Success (('L',n),_,_) -> Some +n
    | Success (('R',n),_,_) -> Some -n
    | _ -> None

let applyTurn (position:int) (clicks:int) = (position+clicks)%100 

let getPassword startPosition inputFile = 
    File.ReadLines inputFile
    |> Seq.choose parseTurn
    |> Seq.scan applyTurn startPosition
    |> Seq.filter ((=) 0)
    |> Seq.length

printfn "Part 1 Result: %d" (getPassword 50 "input.txt")

let turnToClicks (clicks:int) = Seq.replicate (Math.Abs clicks) (Math.Sign clicks)
            
let getPassword2 startPosition inputFile = 
    File.ReadLines inputFile
    |> Seq.choose parseTurn
    |> Seq.collect turnToClicks
    |> Seq.scan applyTurn startPosition
    |> Seq.filter ((=) 0)
    |> Seq.length

printfn "Part 2 Result: %d" (getPassword2 50 "input.txt")