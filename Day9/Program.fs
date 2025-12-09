open System
open System.IO
open FParsec

let point = pint64 .>> pchar ',' .>>. pint64  .>> eof

let parsePoint s =
    match run point s with
    | Success (v,_,_) -> v
    | Failure (error, _, _) ->
        failwithf "Parsing error: %s" error

let readPoints input =
    File.ReadLines(input)
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.map parsePoint
    |> Seq.toList

let tuple a b = (a, b)

let rec pairs list = 
    seq { 
        match list with
        | head::tail -> 
            yield! tail |> Seq.map (tuple head)
            yield! tail |> pairs
        | _ -> ()
    }    

let area ((x1,y1),(x2,y2)) =
    (abs (x1-x2)+1L) * (abs (y1-y2)+1L)

let largestArea points =
    points
    |> pairs
    |> Seq.map area
    |> Seq.max

let result1 input =
    readPoints input
    |> largestArea

printfn "Part 1 Example: %d" (result1 "example.txt")
printfn "Part 1 Result: %d" (result1 "input.txt")


let betweenIncluding (a,b) v =
    v >= (min a b) && v <= (max a b)

let betweenExcluding (a,b) v =
    v > (min a b) && v < (max a b)

let sideOfLine (x:int64,y:int64) ((x0:int64,y0:int64),(x1:int64,y1:int64)) =
    (x1 - x0) * (y - y0) - (y1 - y0) * (x - x0)

let pointIsInsidePolygon polygon (x,y) =
    let mutable windingNumber = 0
    for ((x0,y0),(x1,y1)) in polygon do
        if y >= y0 then
            if y < y1 && sideOfLine (x,y) ((x0,y0), (x1,y1)) > 0 then
                windingNumber <- windingNumber + 1
        else
            if y >= y1 && sideOfLine (x,y) ((x0,y0), (x1,y1)) <= 0 then
                windingNumber <- windingNumber - 1
    windingNumber <> 0

let pointIsOnLine (x,y) ((x0,y0),(x1,y1)) =
    if x0 = x1 then
        x = x0 && betweenIncluding (y0,y1) y
    else
        y = y0 && betweenIncluding (x0,x1) x

let tracePolygon points =
    points @ [points.Head]
    |> List.pairwise
    |> List.toArray

let pointIsOnPolygonPerimiter polygon point =
    polygon |> Array.exists (pointIsOnLine point)

let pointIsInPolygon polygon point =
    pointIsOnPolygonPerimiter polygon point || pointIsInsidePolygon polygon point

let rectCorners ((x0,y0),(x1,y1))  =   
    seq { (x0,y0); (x0,y1); (x1,y0); (x1,y1); (x0,y0) }

let rectLines ((x0,y0),(x1,y1))  =   
    tracePolygon (rectCorners ((x0,y0),(x1,y1)) |> Seq.toList)

let isLineVertical ((x0,y0),(x1,y1)) =
    x0 = x1

let checkVerticalCrossesHorizontal  verticalLine horizontalLine =
    let ((xv,y0), (_,y1)) = verticalLine
    let ((x0,yh), (x1,_)) = horizontalLine
    betweenExcluding (y0,y1) yh && betweenExcluding (x0,x1) xv

let doesRectLineCrossPolygonLine (a,b) (c,d) =
    let abVertical = isLineVertical (a,b)
    let cdVertical = isLineVertical (c,d)
    if abVertical <> cdVertical then
        if abVertical then
            checkVerticalCrossesHorizontal (a,b) (c,d)
        else
            checkVerticalCrossesHorizontal (c,d) (a,b)
    else
        false

let doesRectLineCrossPolygon polygon rectLine  =
    polygon
    |> Seq.exists (doesRectLineCrossPolygonLine rectLine)

let doesRectCrossPolygon polygon rect =
    rectLines rect
    |> Array.exists (doesRectLineCrossPolygon polygon)

let largestRectInsidePolygon points =
    let polygon = tracePolygon points
    let rectCornersInsidePolygon rect = rectCorners rect |> Seq.forall (pointIsInPolygon polygon)
    let rectIsContainedByPolygon rect = not (doesRectCrossPolygon polygon rect)

    points
    |> pairs
    |> Seq.sortByDescending area
    |> Seq.filter rectCornersInsidePolygon
    |> Seq.filter rectIsContainedByPolygon
    |> Seq.head
    |> area

let result2 input =
    readPoints input
        |> largestRectInsidePolygon

printfn "Part 2 Example: Largest Area = %A" (result2 "example.txt")
printfn "Part 2 Result : Largest Area = %A" (result2 "input.txt")
