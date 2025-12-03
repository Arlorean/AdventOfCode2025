open System
open System.IO

let getIndexedMax joltages  =
    joltages
    |> Seq.map (Char.GetNumericValue >> int64)
    |> Seq.indexed
    |> Seq.maxBy snd

let getJoltage (joltages:string) =
    let i,a = getIndexedMax joltages[..joltages.Length-2]
    let _,b = getIndexedMax joltages[i+1..]
    a*10L + b

let getJoltageTotal inputFile = 
    File.ReadAllLines inputFile
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.map getJoltage
    |> Seq.sum

printfn "Part 1 Result: %A" (getJoltageTotal "input.txt")

let getJoltageN n (joltages:string) =
    let mutable J = 0L
    let mutable s = 0
    let mutable e = joltages.Length - n
    for _ in 1..n do
        let i,j = getIndexedMax joltages[s..e]
        J <- J*10L + j
        s <- s + i + 1
        e <- e + 1
    J

let getJoltageNTotal n inputFile = 
    File.ReadAllLines inputFile
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.map (getJoltageN n)
    |> Seq.sum

printfn "Part 2 Result: %A" (getJoltageNTotal 12 "input.txt")
