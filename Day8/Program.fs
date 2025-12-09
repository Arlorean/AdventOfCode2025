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

let shortestPairs input pairCount = 
    readPoints input
    |> pairs
    |> Seq.toArray
    |> Array.sortBy distance
    |> Array.take pairCount

let inline pairToSet (p0, p1) = Set.ofArray [|p0; p1|]

let connectCircuits input pairCount =
    let pairs = shortestPairs input pairCount
    let mutable circuits : Set<(_)>[] = Array.empty
    for i in 0 .. pairs.Length - 1 do
        let i0 = circuits |> Array.tryFindIndex (Set.contains (fst pairs[i]))
        let i1 = circuits |> Array.tryFindIndex (Set.contains (snd pairs[i])) 
        do match i0, i1 with
            | None, None ->
                circuits <- Array.append circuits [|(pairToSet pairs[i])|]
            | Some i0, None ->
                circuits[i0] <- circuits[i0] + (pairToSet pairs[i])
            | None, Some i1 ->
                circuits[i1] <- circuits[i1] + (pairToSet pairs[i])
            | Some i0, Some i1 when i0 <> i1 -> 
                circuits[i0] <- circuits[i0] + circuits[i1]
                circuits <- Array.removeAt i1 circuits
            | _ -> ()
    circuits

let connectCircuitsSum circuits topCount = 
    circuits
    |> Array.map Set.count
    |> Array.sortDescending
    |> Array.take topCount
    |> Array.fold (*) 1

let result1 input pairCount topCount =
    connectCircuits input pairCount
    |> connectCircuitsSum <| topCount

printfn "Part 1 Example: %A" (result1 "example.txt" 10 3)
printfn "Part 1 Result: %A" (result1 "input.txt" 1000 3)
