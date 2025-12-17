open System
open System.IO
open System.Numerics
open FParsec

module Shape =
    [<Literal>]
    let Size = 1
    [<Literal>]
    let Width = 1+Size*2
    [<Literal>]
    let Height = 1+Size*2
    [<Literal>]
    let CellCount = Width * Height
    let EmptySymbol = "🔘" //"╶╴"
    let Symbols = Array.concat [|
        [|"🟥";"🟦";"🟩";"🟨";"🟪";"🟫"|]
        [|"🔴";"🔵";"🟢";"🟡";"🟣";"🟤"|]
        [|"⚪";"⚫";"🔲";"🔳";"🔶";"🔷"|]
    |]
    let GetSymbol i = Symbols[i % Symbols.Length]

type ShapeTypeIndex = 
    | Type0 = 0
    | Type1 = 1
    | Type2 = 2
    | Type3 = 3
    | Type4 = 4
    | Type5 = 5

type Symbol = string

[<StructuredFormatDisplay("{Format}")>]
[<Struct>]
type Shape = { Bits : uint16; } with
    static member (&&&) (shape1,shape2) = { Bits = shape1.Bits &&& shape2.Bits }
    static member (|||) (shape1,shape2) = { Bits = shape1.Bits ||| shape2.Bits }
    static member op_Explicit (shape: Shape) = int shape.Bits

    member this.FullCount = BitOperations.PopCount (uint this.Bits)
    member this.EmptyCount = Shape.CellCount - this.FullCount

    static member CanFit shapeSlot shape = shapeSlot &&& shape = Shape.Empty
    static member GetEmptyCount (shape:Shape) = shape.EmptyCount
    static member GetFullCount (shape:Shape) = shape.FullCount


    member this.Item
        with get (row : int, col : int) = this.GetCell (row,col)

    member this.Item
        with get (rows: Range, cols: Range) = this.GetCellRange (rows, cols)

    member this.GetCell (row,col) =
        if row < 0 || row >= Shape.Height || col < 0 || col >= Shape.Width then false
        else this.Bits &&& (Shape.CellBitMask row col) <> 0us

    member this.GetCell2 row col =
        this.GetCell (row,col)

    member this.GetCellRange (rows: Range, cols: Range) =
        let rs =
            let s = if rows.Start.IsFromEnd then Shape.Height - rows.Start.Value else rows.Start.Value
            let e = if rows.End.IsFromEnd then Shape.Height - rows.End.Value else rows.End.Value
            s, e

        let cs =
            let s = if cols.Start.IsFromEnd then Shape.Width - cols.Start.Value else cols.Start.Value
            let e = if cols.End.IsFromEnd then Shape.Width - cols.End.Value else cols.End.Value
            s, e

        let (r0, r1), (c0, c1) = rs, cs

        Array2D.init (r1 - r0) (c1 - c0) this.GetCell2

    member this.GetCellRow row =
        Array.init Shape.Width (this.GetCell2 row)

    static member CellBitMask row col = 1us <<< (row * Shape.Width + col)

    static member CellOffsets =
        seq {
            for rowOffset in 0..Shape.Height-1 do
                for colOffset in 0..Shape.Width-1 do
                    yield (rowOffset, colOffset)
        }

    static member AdjacentCellOffsets = 
        seq {
            for rowOffset in -(Shape.Height-1)..Shape.Height-1 do
                for colOffset in -(Shape.Width-1)..Shape.Width-1 do
                    yield (rowOffset, colOffset)
        }


    static member Init getCell =
        let mutable bits: uint16 = 0us
        for row, col in Shape.CellOffsets do
            if getCell (row,col) then
                bits <- bits ||| Shape.CellBitMask row col
        { Bits = bits }

    static member Create(cells:bool[][]) = 
        let cell (r,c) = cells[r][c]
        Shape.Init cell

    static member New(bits:uint16) = { Bits = bits }

    static member Empty = Shape.New 0us
    static member Full = Shape.New 0x1FFus

    static member FormatLines symbol (this:Shape)  =
        let getCellRow = this.GetCellRow
        let cellString b = if b then symbol else Shape.EmptySymbol
        let rowString n = getCellRow n |> Array.map cellString |> String.Concat
        [|
            for row in 0..Shape.Height-1 do
                yield rowString row
        |]
    member this.Format =  String.concat "\n" (Shape.FormatLines (Shape.GetSymbol (int this)) this)
    override this.ToString() = this.Format

    member this.Rotate90 = 
        let getCell  = this.GetCell
        Shape.Init (fun (r,c) -> getCell ((Shape.Width-1)-c,r))
    member this.FlipHorizontal =
        let getCell  = this.GetCell
        Shape.Init (fun (r,c) -> getCell (r,(Shape.Width-1)-c))
    member this.FlipVertical =
        let getCell  = this.GetCell
        Shape.Init (fun (r,c) -> getCell ((Shape.Height-1)-r,c))

    static member AllOrientations (shape:Shape) =
        let r0 = shape
        let r90 = r0.Rotate90
        let r180 = r90.Rotate90
        let r270 = r180.Rotate90
        let fH = r0.FlipHorizontal
        let fV = r0.FlipVertical
        let fH90 = fH.Rotate90
        let fV90 = fV.Rotate90
        [| r0; r90; r180; r270; fH; fV; fH90; fV90 |] |> Array.distinct

[<StructuredFormatDisplay("{Width}x{Height}: {ShapeTypeCounts}")>]
type Region = {
    Width: int
    Height: int
    ShapeTypeCounts: int[]
} with
    static member Create((width, height), shapeTypeCounts) =
        { Width = width; Height = height; ShapeTypeCounts = shapeTypeCounts }
    member this.LastShapeRow = this.Height-Shape.Height
    member this.LastShapeCol = this.Width-Shape.Width


let region = pint32 .>> pchar 'x' .>>. pint32 .>> pchar ':' .>>. parray 6 (spaces >>. pint32) .>> eof |>> Region.Create

let parseRegion s =
    match run region s with
    | Success (region,_,_) -> region
    | Failure (error,_,_) -> failwith error

let isShapeCellFull c = c = '#'

let shapeRow (line:string) =
    line[0..(Shape.Width)] |> Seq.map isShapeCellFull |> Seq.toArray

let readShape (lines:string[]) = 
    lines[1..Shape.Height] |> Seq.map shapeRow |> Seq.toArray |> Shape.Create

let readShapes lines =
    let shapeCount = 6
    let shapeLineCount = 5
    [|
        for i in 0..shapeCount-1 do
            yield readShape (Array.sub lines (i * shapeLineCount) shapeLineCount)
    |]

let readRegions (lines:string[]) =
    lines
    |> Array.map parseRegion

// Map from shape slot (by index (bits)) to possible shapes that fit into that shape slot
type ShapeSlotOptions = { ShapeSlots:(Shape*ShapeTypeIndex)[][] } with
    member this.GetShapesThatFit (shapeSlot:Shape) = this.ShapeSlots[int shapeSlot]

    static member Create (shapeTypes:Shape[]) =
        let mutable allPossibleShapes = []
        for i in 0..shapeTypes.Length-1 do
            for orientedShape in Shape.AllOrientations shapeTypes[i] do
                allPossibleShapes <- (orientedShape, enum<ShapeTypeIndex> i) :: allPossibleShapes

        let shapesThatFitSlot (shapeSlot:Shape) =
            allPossibleShapes
            |> Seq.filter (fst >> Shape.CanFit shapeSlot)
            |> Seq.sortBy (fst >> Shape.GetFullCount)
            |> Seq.toArray 
        { ShapeSlots = ShapeSlotOptions.ShapeSlotKeys |> Array.map shapesThatFitSlot } 

    static member ShapeSlotKeys = [|0us..0x1FFus|] |> Array.map Shape.New

let addTuples (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

[<StructuredFormatDisplay("{Format}")>]
type RegionState = {
    // Static state about the region and shape types
    Region : Region
    ShapeSlotOptions: ShapeSlotOptions
    ShapeTypes : Shape[]

    // Dynamic state about the current placement of shapes in the region
    ShapeTypeCounts: int[]
    PlacedShapes: Map<int*int, (Shape*Symbol)>
    ShapeSlots: Map<int*int, (Shape*Symbol)>
    EmptySlots: Set<int*int>
    Cells: Set<int*int>
} with
    member this.GetCell (row:int, col:int) =
        this.Cells.Contains (row, col)

    member this.Item
        with get key = this.GetCell key

    member this.ShapeTypeCountTotal = this.ShapeTypeCounts |> Array.sum

    member this.NextShapeSymbol = Shape.GetSymbol this.PlacedShapes.Count

    member this.PlaceShape (row:int,col:int) (shape:Shape,shapeTypeIndex:ShapeTypeIndex) =
        let newCells = 
            Shape.CellOffsets
            |> Seq.filter shape.GetCell
            |> Seq.map (addTuples (row, col))
            |> Set.ofSeq
            |> (+) this.Cells

        let symbol = Shape.GetSymbol this.PlacedShapes.Count
        let mutable newShapeSlots = this.ShapeSlots
        let mutable newEmptySlots = this.EmptySlots
        for (rowoffset,colOffset) in Shape.AdjacentCellOffsets do
            let (r,c) = (row + rowoffset, col + colOffset)
            if r >= 0 && r <= this.Region.LastShapeRow && c >= 0 && c <= this.Region.LastShapeCol then
                let getNewCell (rowOffset,colOffset) = newCells.Contains (r + rowOffset, c + colOffset)
                let shapeSlot = Shape.Init getNewCell
                newShapeSlots <- newShapeSlots.Add ((r,c), (shapeSlot, symbol))
                newEmptySlots <- newEmptySlots.Remove (r,c)

        let newShapeTypeCounts = Array.copy this.ShapeTypeCounts
        newShapeTypeCounts[int shapeTypeIndex] <- newShapeTypeCounts[int shapeTypeIndex] - 1
        
        {
            Region = this.Region
            PlacedShapes = this.PlacedShapes.Add ((row, col), (shape, symbol))
            ShapeSlots = newShapeSlots
            Cells = newCells
            EmptySlots = newEmptySlots
            ShapeTypes = this.ShapeTypes
            ShapeTypeCounts = newShapeTypeCounts
            ShapeSlotOptions = this.ShapeSlotOptions
        }

    member this.GetAvailableShapesTypes shapeSlot =
        this.ShapeSlotOptions.GetShapesThatFit shapeSlot
        |> Array.filter (fun (_, shapeType) -> this.ShapeTypeCounts[int shapeType] > 0)

    static member New region shapeTypes = { 
        Region = region
        ShapeSlotOptions = ShapeSlotOptions.Create shapeTypes
        ShapeTypes = shapeTypes

        ShapeTypeCounts = region.ShapeTypeCounts
        PlacedShapes = Map.empty
        ShapeSlots = Map.empty
        Cells = Set.empty
        EmptySlots = 
            seq {
                for row in 0..region.LastShapeRow do
                    for col in 0..region.LastShapeCol do
                        yield (row, col)
            } |> Set.ofSeq
    }

    member this.CellStrings =
        let mutable cells = Array2D.create this.Region.Height this.Region.Width Shape.EmptySymbol
        let setCell (symbol:string) (row:int, col:int) = cells[row,col] <- symbol
        let setShapeCells ((row:int, col:int),(shape:Shape,symbol:Symbol)) =
            Shape.CellOffsets
            |> Seq.filter shape.GetCell
            |> Seq.map (addTuples (row, col))
            |> Seq.iter (setCell symbol)

        this.PlacedShapes |> Map.toSeq |> Seq.iter setShapeCells
        cells

    member this.FormatLines =
        [|
            let cellStrings = this.CellStrings
            for row in 0..cellStrings.GetLength(0)-1 do
                yield cellStrings[row, *] |> String.Concat 
        |]

    member this.Format = this.FormatLines |> String.concat "\n"

let addBorder (lines:string[]) =
    let width = lines[0].Length
    let border = String('─', width)
    [|
        yield "┌" + border + "┐"
        for line in lines do
            yield "│" + line + "│"
        yield "└" + border + "┘"
    |]

let printOptions (linesForOptions:string[][]) =
    let borderedOptions = linesForOptions |> Array.map addBorder
    for i in 0..borderedOptions[0].Length-1 do
        for optionLines in borderedOptions do
            printf "%s  " optionLines[i]
        printfn ""
    printfn ""

let printSingleOption optionLines = printOptions [|optionLines|]

let tryFitShapes shapeTypes (region:Region) =
    let mutable regionState: RegionState = RegionState.New region shapeTypes

    // Deliberately place the first shape in the top-left corner
    let firstShapeTypeIndex = 
        regionState.ShapeTypeCounts
        |> Array.mapi (fun i count -> (i, count))
        |> Array.filter (fun (_, count) -> count > 0)
        |> Array.head
        |> (fst >> enum<ShapeTypeIndex>)
    regionState <- regionState.PlaceShape (0, 0) (shapeTypes[int firstShapeTypeIndex], firstShapeTypeIndex)

    let mutable shapePlaced = true
    while shapePlaced && regionState.ShapeTypeCountTotal > 0 do
        printSingleOption regionState.FormatLines
        printfn "%A" regionState.ShapeTypeCounts

        let placeShapeOptions =
            regionState.ShapeSlots
            |> Map.toSeq
            |> Seq.map (fun ((row,col),(shapeSlot,_)) -> (row,col),regionState.GetAvailableShapesTypes shapeSlot)
            |> Seq.filter (fun (_, shapeOptions) -> shapeOptions.Length > 0)

        if (placeShapeOptions |> Seq.isEmpty) then
            if (regionState.EmptySlots |> Seq.isEmpty) then
                printfn "No place shape options available, stopping."
                shapePlaced <- false          
            else
                let (row: int,col) = (regionState.EmptySlots |> Seq.head)
                let shapeOptions = (regionState.GetAvailableShapesTypes Shape.Empty)
                let (shape,shapeTypeIndex) = shapeOptions[0]
                printfn "No place shape options, placing first available shape in first empty slot:\n"
                printfn "  At (%d,%d): %d options (only showing first)" row col shapeOptions.Length

                printSingleOption (Shape.FormatLines regionState.NextShapeSymbol shape)

                regionState <- regionState.PlaceShape (row,col) (shape,shapeTypeIndex)
        else
            printfn "Place shape options: %d" (placeShapeOptions |> Seq.length)
            for ((row,col), shapeOptions) in placeShapeOptions do
                printfn "  At (%d,%d): %d options" row col shapeOptions.Length
                shapeOptions |> Array.map fst |> Array.map (Shape.FormatLines regionState.NextShapeSymbol) |> printOptions
            
            let ((row: int,col), shapeTypeTuples) = 
                placeShapeOptions
                |> Seq.minBy (fun (_, shapeTypeTuples) -> shapeTypeTuples.Length)
            regionState <- regionState.PlaceShape (row,col) shapeTypeTuples[0]

    regionState.ShapeTypeCounts

let result1 input = 
    let lines = File.ReadAllLines(input)
    let shapes = readShapes lines
    let regions = readRegions lines[30..]

    let shapeString i (shape:Shape) = Shape.FormatLines Shape.Symbols[i] shape
    let shapeStrings = shapes |> Array.mapi shapeString
    printfn "Shapes:"
    printOptions shapeStrings

    // regions
    // |> Array.map (tryFitShapes shapes)
    regions
    |> Array.skip 1 |> Array.head |> tryFitShapes shapes


printfn "Part 1 Example: %A" (result1 "example.txt")
//printfn "Part 1 Result: %A" (result1 "input.txt")

