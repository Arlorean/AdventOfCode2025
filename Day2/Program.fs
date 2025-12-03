open System
open System.IO
open FParsec

let range = pint64 .>> pstring "-" .>>. pint64
let parser = sepBy range (pstring ",") .>> spaces .>> eof

let rangeToSeq (startId, endId) = seq { startId .. endId }

let parseRanges s =
    match run parser s with
    | Success (ranges,_,_) -> ranges 
    | Failure (error, _, _) ->
        printfn "Parsing error: %s" error
        []

let isInvalidId id = 
    let s = string id
    if s.Length % 2 = 0 then
        let a = s.[..(s.Length/2-1)]
        let b = s.[(s.Length/2)..]
        a = b
    else 
        false

let getInvalidIdTotal inputFile = 
    File.ReadAllText inputFile
    |> parseRanges
    |> Seq.collect rangeToSeq
    |> Seq.filter isInvalidId
    |> Seq.sum

printfn "Part 1 Result: %d" (getInvalidIdTotal "input.txt")

let split (s:string) n = [|
        for i: int in 0..n..s.Length-1 do
            yield s.Substring(i, n)
    |]

let isInvalidId2 id = 
    let s = string id
    seq {
        for i in 1..(s.Length/2) do
            if s.Length % i = 0 then
                let parts = split s i
                if parts |> Array.forall ((=) parts[0]) then
                    yield true
    }
    |> Seq.isEmpty
    |> not

let getInvalidIdTotal2 inputFile = 
    File.ReadAllText inputFile
    |> parseRanges
    |> Seq.collect rangeToSeq
    |> Seq.filter isInvalidId2
    |> Seq.sum

printfn "Part 2 Result: %d" (getInvalidIdTotal2 "input.txt")
