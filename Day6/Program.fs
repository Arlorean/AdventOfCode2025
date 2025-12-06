open System
open System.IO

let tokenize (s:string) = s.Split(' ', StringSplitOptions.RemoveEmptyEntries)

let rows inputFile =
    File.ReadAllLines inputFile
        |> Array.filter ((<>) "")
        |> Array.map tokenize

let getColumn col row =
    Array.get row col

let evalColumn (rows:string[][]) col =
    let lastRow = rows.Length - 1;
    let fn = 
        match rows[lastRow][col] with
        | "+" -> (+)
        | "*" -> (*)
        | _ -> failwith "Unknown operator"
    rows[..lastRow-1]
        |> Array.map (getColumn col)
        |> Array.map Int64.Parse
        |> Array.reduce fn

let result inputFile =
    let rows = rows inputFile
    let numCols = rows[0].Length
    [|0 .. numCols - 1|]
        |> Array.map (evalColumn rows)
        |> Array.reduce (+)

printfn "Part 1 Result: %A" (result "input.txt")

