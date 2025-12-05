open System
open System.IO
open System.Diagnostics

module Array2D =
    let indices array =
        seq {
            for r in 0 .. Array2D.length1 array - 1 do
                for c in 0 .. Array2D.length2 array - 1 do
                    yield r, c
        }

    let get2 array (r,c) =
        Array2D.get array r c

    let set2 array value (r,c)  =
        Array2D.set array r c value

    let flatten array =
        indices array
        |> Seq.map (get2 array)


let printGrid (grid: char[,]) =
    for r in 0 .. Array2D.length1 grid - 1 do
        for c in 0 .. Array2D.length2 grid - 1 do
            printf "%c" grid[r,c]
        printfn ""

let getFileAsGrid inputFile =
    let lines = File.ReadAllLines inputFile |> Array.filter (String.IsNullOrWhiteSpace >> not)
    let rows = lines.Length
    let cols = lines[0].Length
    let charFromLines r c = lines[r][c]
    Array2D.init lines.Length lines[0].Length charFromLines

let get3x3 (grid: char[,]) (r, c) =
    grid[r-1..r+1, c-1..c+1]
    |> Array2D.flatten

let isPaperRoll c = c = '@'

let isAccessible grid n (r,c) =
    get3x3 grid (r,c)
    |> Seq.filter isPaperRoll
    |> Seq.length <= n

let getAccessibleRolls maxAdjacentRolls grid = 
    Array2D.indices grid
    |> Seq.filter (Array2D.get2 grid >> isPaperRoll)
    |> Seq.filter (isAccessible grid maxAdjacentRolls)

let getAccessibleRollsCount maxAdjacentRolls grid = 
    grid 
    |> getAccessibleRolls maxAdjacentRolls
    |> Seq.length

let grid1 = getFileAsGrid "input.txt"

printfn "Part 1 Result: %A" (getAccessibleRollsCount 4 grid1)


let removeRolls grid rolls =
    rolls |> Seq.iter (Array2D.set2 grid '.')

let rec removeAccessibleRollsLoop maxAdjacentRolls grid total = 
    let accessibleRolls = getAccessibleRolls maxAdjacentRolls grid |> Seq.toArray
    if Array.isEmpty accessibleRolls then
        total
    else
        removeRolls grid accessibleRolls
        removeAccessibleRollsLoop maxAdjacentRolls grid (total + Array.length accessibleRolls)

printfn "Part 2 Result: %A" (removeAccessibleRollsLoop 4 grid1 0)
