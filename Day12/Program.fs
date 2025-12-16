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

    member this.FormatLines symbol =
        let getCellRow = this.GetCellRow
        let cellString b = if b then symbol else Shape.EmptySymbol
        let rowString n = getCellRow n |> Array.map cellString |> String.Concat
        [|
            yield ("┌" + String('─',Shape.Width*2) + "┐")
            for row in 0..Shape.Height-1 do
                yield ("│" + rowString row + "│")
            yield ("└" + String('─',Shape.Width*2) + "┘")
        |]
    member this.Format =  String.concat "\n" (this.FormatLines Shape.Symbols[0])
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
type ShapeSlots = { ShapeSlots:Shape[][] } with
    member this.GetShapesThatFit (shapeSlot:Shape) = this.ShapeSlots[int shapeSlot]

    static member Create (shapes:Shape[]) =
        let shapesThatFitSlot (shapes:Shape[]) (shapeSlot:Shape) =
            shapes
            |> Seq.filter (Shape.CanFit shapeSlot)
            |> Seq.sortBy Shape.GetFullCount
            |> Seq.toArray 
        { ShapeSlots = ShapeSlots.ShapeSlotKeys |> Array.map (shapesThatFitSlot shapes) } 

    static member ShapeSlotKeys = [|0us..0x1FFus|] |> Array.map Shape.New

let addTuples (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

[<StructuredFormatDisplay("{Format}")>]
type RegionState = {
    Region : Region;
    PlacedShapes: Map<int*int, Shape>;
    ShapeSlots: Map<int*int, Shape>;
    Cells: Set<int*int>;
    ShapeTypeCounts: int[];
} with
    member this.GetCell (row:int, col:int) =
        this.Cells.Contains (row, col)

    member this.Item
        with get key = this.GetCell key

    member this.ShapeTypeCountTotal = this.ShapeTypeCounts |> Array.sum

    member this.PlaceShape (row:int,col:int) (shape:Shape) (shapeType:int) =
        let newCells = 
            Shape.CellOffsets
            |> Seq.filter shape.GetCell
            |> Seq.map (addTuples (row, col))
            |> Set.ofSeq

        let mutable newShapeSlots = this.ShapeSlots
        for rowoffset,colOffset in Shape.AdjacentCellOffsets do
            let r,c = row + rowoffset, col + colOffset
            if r >= 0 && r <= this.Region.Height-2 && c >= 0 && c <= this.Region.Width-2 then
                let getNewCell (rowOffset,colOffset) = newCells.Contains (r + rowOffset, c + colOffset)
                let shapeSlot = Shape.Init getNewCell
                newShapeSlots <- newShapeSlots.Add ((r,c), shapeSlot)

        let newShapeTypeCounts = Array.copy this.ShapeTypeCounts
        newShapeTypeCounts[shapeType] <- newShapeTypeCounts[shapeType] - 1
        
        { this with
            PlacedShapes = this.PlacedShapes.Add ((row, col), shape)
            ShapeSlots = newShapeSlots
            Cells = this.Cells + newCells
            ShapeTypeCounts = newShapeTypeCounts
        }

    member this.GetShapeSlot (row:int, col:int) =
        match this.ShapeSlots.TryFind (row, col) with
        | Some shapeSlot -> shapeSlot
        | None -> Shape.Empty

    static member New region = { Region = region; PlacedShapes = Map.empty; ShapeSlots = Map.empty; Cells = Set.empty; ShapeTypeCounts = region.ShapeTypeCounts }

    member this.CellStrings =
        let mutable i = 0
        let mutable cells = Array2D.create this.Region.Height this.Region.Width Shape.EmptySymbol
        let setCell (symbol:string) (row:int, col:int) = cells[row,col] <- symbol
        let setShapeCells (row:int, col:int) (shape:Shape) =
            let symbol = Shape.Symbols[i % Shape.Symbols.Length]
            i <- i + 1
            Shape.CellOffsets
            |> Seq.filter shape.GetCell
            |> Seq.map (addTuples (row, col))
            |> Seq.iter (setCell symbol)

        this.PlacedShapes |> Map.iter setShapeCells
        cells

    member this.FormatLines =
        [|
            let cellStrings = this.CellStrings
            for row in 0..cellStrings.GetLength(0)-1 do
                yield cellStrings[row, *] |> String.Concat 
        |]

    member this.Format = this.FormatLines |> String.concat "\n"

let regionStateToString (regionState: RegionState) =
    let lines = regionState.FormatLines
    seq {
        yield ("┌" + String('─',regionState.Region.Width*2) + "┐\n")
        for row in 0..(lines.Length - 1) do
            yield ("│" + lines[row] + "│\n")
        yield ("└" + String('─',regionState.Region.Width*2) + "┘\n")
    }       

let tryFitShapes shapes (region:Region) =
    let shapeOrientations =
        shapes
        |> Array.map Shape.AllOrientations

    // Array of all possible shapes to which shape orientations will fit into that shape
    let allPossibleShapes = shapeOrientations |> Array.concat
    let shapeSlots = ShapeSlots.Create allPossibleShapes

    let shapeTypeToIndex =
        let shapeTypes = Array.create shapeSlots.ShapeSlots.Length None
        let addShapeType shapeType shape =
            Shape.AllOrientations shape
            |> Array.iter (
                fun orientedShape -> 
                    shapeTypes[int orientedShape] <- Some shapeType
                    //printfn "Mapping shape %d to type %d" (int orientedShape) shapeType
               )

        shapes
        |> Array.iteri addShapeType

        shapeTypes
    
    // Get the original shape type for a given shape orientation
    let getShapeType shape = 
        match shapeTypeToIndex[int shape] with
        | Some shapeType -> Some (shape,shapeType)
        | _ -> None

    let mutable regionState: RegionState = RegionState.New region

    let addShape row col shape shapeType =
        regionState <- regionState.PlaceShape (row,col) shape shapeType
        regionStateToString regionState
        |> Seq.iter (printf "%s")
        printfn "%A" regionState.ShapeTypeCounts

    printfn "%A" regionState.ShapeTypeCounts

    // Deliberately place the first shape in the top-left corner
    let firstShapeType = 
        regionState.ShapeTypeCounts
        |> Array.mapi (fun i count -> (i, count))
        |> Array.filter (fun (_, count) -> count > 0)
        |> Array.head
        |> fst
    addShape 0 0 shapes[firstShapeType] firstShapeType

    for row in 0..(region.Height - Shape.Width) do
        for col in 01..(region.Width - Shape.Height) do
            if regionState.ShapeTypeCountTotal = 0 then ()
            else 
                let shapeSlot = regionState.GetShapeSlot (row, col)
                printfn "Trying to fit shape at (%d,%d) into slot:\n%s" row col (shapeSlot.Format)
                let possibleShapes = shapeSlots.GetShapesThatFit shapeSlot
                
                let bestFitShapeType = 
                    possibleShapes
                    |> Array.choose getShapeType
                    |> Array.filter (fun (shape2, shapeType) -> regionState.ShapeTypeCounts[shapeType] > 0)
                    |> Array.tryHead

                match bestFitShapeType with
                | Some (shape, shapeType) -> addShape row col shape shapeType
                | _ -> ()

    regionState.ShapeTypeCounts


let result1 input = 
    let lines = File.ReadAllLines(input)
    let shapes = readShapes lines
    let regions = readRegions lines[30..]

    let shapeString i (shape:Shape) = shape.FormatLines Shape.Symbols[i]
    let shapeStrings = shapes |> Array.mapi shapeString
    printfn "Shapes:"
    for i in 0..Shape.Height+1 do
        for shapeString in shapeStrings do
            printf "%s  " shapeString[i]
        printfn ""
    printfn ""


    regions
    |> Array.map (tryFitShapes shapes)
    // regions
    // |> Array.head |>  tryFitShapes shapes


printfn "Part 1 Example: %A" (result1 "example.txt")
//printfn "Part 1 Result: %A" (result1 "input.txt")

