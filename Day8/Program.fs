open System
open System.IO
open FParsec

let point = pint64 .>> pchar ',' .>>. pint64 .>> pchar ',' .>>. pint64 .>> eof

let parsePoint s =
    match run point s with
    | Success (((x,y),z),_,_) -> (x,y,z)
    | Failure (error, _, _) ->
        failwithf "Parsing error: %s" error

let readPoints input =
    File.ReadLines(input)
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.map parsePoint
    |> Seq.toList

let sqr x = x*x

let distance ((x1,y1,z1),(x2,y2,z2)) =
    sqrt <| float (sqr (x1-x2) + sqr (y1-y2) + sqr (z1-z2))

let tuple a b = (a, b)

let rec pairs list = 
    seq { 
        match list with
        | head::tail -> 
            yield! tail |> Seq.map (tuple head)
            yield! tail |> pairs
        | _ -> ()
    }    

let pairsOrderedByDistance input = 
    readPoints input
    |> pairs
    |> Seq.toArray
    |> Array.sortBy distance

let connectCircuits (pairs : ((int64*int64*int64)*(int64*int64*int64))[]) =
    let mutable circuits= Array.empty
    for i in 0 .. pairs.Length - 1 do
        let (p0,p1) = pairs[i]
        let i0 = circuits |> Array.tryFindIndex (List.contains p0)
        let i1 = circuits |> Array.tryFindIndex (List.contains p1) 
        do match i0, i1 with
            | None, None ->
                circuits <- Array.append circuits [| [p0; p1] |]
            | Some i0, None ->
                circuits[i0] <- p0::p1::List.except [p0] circuits[i0]
            | None, Some i1 ->
                circuits[i1] <- p0::p1::List.except [p1] circuits[i1]
            | Some i0, Some i1 when i0 <> i1 -> 
                circuits[i0] <- p0::p1::(List.except [p0] circuits[i0] @ List.except [p1] circuits[i1])
                circuits <- Array.removeAt i1 circuits
            | _ -> ()
    circuits

let result1 input pairCount topCount = 
    pairsOrderedByDistance input
    |> Array.take pairCount
    |> connectCircuits
    |> Array.map List.length
    |> Array.sortDescending
    |> Array.take topCount
    |> Array.fold (*) 1

printfn "Part 1 Example: %A" (result1 "example.txt" 10 3)
printfn "Part 1 Result: %A" (result1 "input.txt" 1000 3)


let result2 input = 
    let completeCircuit =
        pairsOrderedByDistance input
        |> connectCircuits
        |> Array.head
    match completeCircuit with
    | (x0,_,_)::(x1,_,_)::_ -> x0 * x1
    | _ -> failwith "Not enough points in circuit"


printfn "Part 2 Example: %A" (result2 "example.txt")
printfn "Part 2 Input: %A" (result2 "input.txt")
