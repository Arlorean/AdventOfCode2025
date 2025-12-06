open System
open System.IO
open System.Text.RegularExpressions

let trim (s:string) = s.Trim()
let getValue (m: Match) = m.Value

let getColumnWidths fnRow = 
    Regex(@"[+*]\s+").Matches fnRow
    |> Seq.map getValue
    |> Seq.map String.length
    |> Seq.toArray

let splitRowByColWidth widths (row:string) =
    [| 
        let mutable index = 0
        for w in widths do
            yield row.Substring(index, w)
            index <- index + w
    |]

let rows inputFile =
    let lines = 
        File.ReadAllLines inputFile
        |> Array.filter (not << String.IsNullOrWhiteSpace)
    let widths = getColumnWidths lines[lines.Length - 1]
    lines
        |> Array.map (splitRowByColWidth widths)

let getIndex i array = Array.get array i


let getFn s =
    match s with
    | "+" -> (+)
    | "*" -> (*)
    | _ -> failwith "Unknown operator"

let evalColumn (rows:string[][]) col =
    let lastRow = rows.Length - 1
    let fn = getFn (rows[lastRow][col] |> trim)
    rows[..lastRow-1]
        |> Array.map (getIndex col >> trim)
        |> Array.map Int64.Parse
        |> Array.reduce fn

let result inputFile =
    let rows = rows inputFile
    let numCols = rows[0].Length
    [|0 .. numCols - 1|]
        |> Array.map (evalColumn rows)
        |> Array.reduce (+)

printfn "Part 1 Result: %A" (result "input.txt")


let evalColumn2 (rows:string[][]) col =
    let lastRow = rows.Length - 1
    let fn = getFn (rows[lastRow][col] |> trim)
    rows[..lastRow-1]
        |> Array.map (getIndex col)
        |> Array.map Seq.toArray // Transpose the
        |> Array.transpose       // digits of the 
        |> Array.map String      // column numbers
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> Array.map Int64.Parse
        |> Array.reduce fn

let result2 inputFile =
    let rows = rows inputFile
    let numCols = rows[0].Length
    [|0 .. numCols - 1|]
        |> Array.map (evalColumn2 rows)
        |> Array.reduce (+)

printfn "Part 2 Result: %A" (result2 "input.txt")
