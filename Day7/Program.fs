open System
open System.IO
open System.Collections.Generic

let rows inputFile =
    File.ReadAllLines inputFile
    |> Array.filter (not << String.IsNullOrWhiteSpace)

let splitter (beams:char[]) (manifold:string) =
    let mutable splitCount = 0
    for i in 0 .. manifold.Length - 1 do
        match beams[i],manifold[i] with
        | '|','^' -> beams[i-1..i+1] <- "|^|".ToCharArray(); splitCount <- splitCount + 1
        | '|','.' -> beams[i] <- '|'
        | 'S','.' -> beams[i] <- '|'
        | 'S','S' -> beams[i] <- 'S'
        | _ -> ()
    (String beams, splitCount)

let folder (beamMap:string, totalSplitCount) (beams:string,splitCount) =
    ( String.Join("\n",[|beamMap;beams|]), totalSplitCount + splitCount )

let result input = 
    let manifolds = rows input
    let beams = manifolds[0].ToCharArray()
    manifolds
    |> Array.map (splitter beams)
    |> Array.fold folder ("",0)

let (beamMap, splitCount) = result "input.txt"
printfn "Part 1 Result: %d splits:%s" splitCount beamMap


let traverseTimelines (row,col) (manifolds:string[]) =
    let cache = Dictionary<struct(int*int), int64>()
    let rec traverse (row,col) =
        let key = struct(row, col)
        if not (cache.ContainsKey key) then
            cache[key] <-
                if row >= manifolds.Length then
                    1L
                else
                    if manifolds[row][col] = '^' then
                        traverse (row,col-1) + traverse (row,col+1)
                    else
                        traverse (row+1,col)
        cache[key]
    traverse (row,col)

let result2 input = 
    let manifolds =
        rows input
    manifolds 
        |> traverseTimelines (1,manifolds[0].IndexOf 'S')

printfn "Part 2 Result: %d timelines" (result2 "input.txt")
 