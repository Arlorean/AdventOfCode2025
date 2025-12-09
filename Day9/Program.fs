open System
open System.IO
open FParsec

let point = pint64 .>> pchar ',' .>>. pint64  .>> eof

let parsePoint s =
    match run point s with
    | Success (v,_,_) -> v
    | Failure (error, _, _) ->
        failwithf "Parsing error: %s" error

let readPoints input =
    File.ReadLines(input)
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.map parsePoint
    |> Seq.toList

let tuple a b = (a, b)

let rec pairs list = 
    seq { 
        match list with
        | head::tail -> 
            yield! tail |> Seq.map (tuple head)
            yield! tail |> pairs
        | _ -> ()
    }    

let area ((x1,y1),(x2,y2)) =
    abs (x1 - x2+1L) * abs (y1 - y2+1L)

let largestArea points =
    points
    |> pairs
    |> Seq.map area
    |> Seq.max

let result1 input =
    readPoints input
    |> largestArea

printfn "Part 1 Example: %d" (result1 "example.txt")
printfn "Part 1 Result: %d" (result1 "input.txt")

