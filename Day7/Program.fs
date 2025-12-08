open System
open System.IO

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

