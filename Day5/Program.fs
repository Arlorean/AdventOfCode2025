open System
open System.IO

type IngredientID = Int64

let parseRange (s:string) = 
    let parts = s.Split('-')
    ( IngredientID.Parse parts[0], IngredientID.Parse parts[1] )

let parseRanges lines =
    lines |> Array.map parseRange

let parseIngredient (s:string) = 
    IngredientID.Parse s

let parseIngredients lines =
    lines |> Array.map parseIngredient

let isFreshIngredient id (lower,upper) = 
    id >= lower && id <= upper

let isInFreshIngredients freshRanges id = 
    freshRanges |> Array.exists (isFreshIngredient id)

let getFreshIngredients inputFile = 
    let lines = File.ReadAllLines inputFile
    let blankLineIndex = Array.findIndex ((=) "") lines
    let freshRanges = lines[..blankLineIndex-1] |> parseRanges
    let ingredients = lines[blankLineIndex+1..] |> parseIngredients
    ingredients |> Seq.filter (isInFreshIngredients freshRanges)

let numFreshIgredients inputFile =
    getFreshIngredients inputFile |> Seq.length

printfn "Part 1 Result: %A" (numFreshIgredients "input.txt")

let rangeFolder (nextFreshId, total) (lower,upper) =
    if upper < nextFreshId then
        (nextFreshId, total)
    else
        let lower' = max nextFreshId lower
        (upper+1L, total + (upper - lower' + 1L))

let getAllFreshIngredients inputFile = 
    let lines = File.ReadAllLines inputFile
    let blankLineIndex = Array.findIndex ((=) "") lines
    let freshRanges = lines[..blankLineIndex-1] |> parseRanges
    freshRanges
        |> Array.sortBy fst
        |> Array.fold rangeFolder (0L, 0L)
        |> snd

printfn "Part 2 Result: %A" (getAllFreshIngredients "input.txt")
