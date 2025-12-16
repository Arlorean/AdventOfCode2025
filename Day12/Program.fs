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
        this.Bits &&& (Shape.CellBitMask row col) <> 0us

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
            for rowOffset in 0..Shape.Width-1 do
                for colOffset in 0..Shape.Height-1 do
                    yield (rowOffset, colOffset)
        }

    static member Init getCell =
        let mutable bits: uint16 = 0us
        for row, col in Shape.CellOffsets do
            if getCell row col then
                bits <- bits ||| Shape.CellBitMask row col
        { Bits = bits }

    static member Create(cells:bool[][]) = 
        let cell r c = cells[r][c]
        Shape.Init cell

    static member New(bits:uint16) = { Bits = bits }

    static member Empty = Shape.New 0us
    static member Full = Shape.New 0x1FFus

    override this.ToString() = this.Format
    member this.Format = this.FormatSymbol Shape.Symbols[0]
    member this.FormatSymbol symbol =
        let getCellRow = this.GetCellRow
        let cell b = if b then symbol else Shape.EmptySymbol
        let row n = "│" + (getCellRow n |> Array.map cell |> String.Concat) + "│\n"
        ("┌" + String('─',Shape.Width*2) + "┐\n") +
        ([|0..Shape.Height-1|] |> Array.map row |> String.Concat) +  
        ("└" + String('─',Shape.Width*2) + "┘\n")
    member this.Rotate90 = 
        let getCell  = this.GetCell
        Shape.Init (fun r c -> getCell ((Shape.Width-1)-c,r))
    member this.FlipHorizontal =
        let getCell  = this.GetCell
        Shape.Init (fun r c -> getCell (r,(Shape.Width-1)-c))
    member this.FlipVertical =
        let getCell  = this.GetCell
        Shape.Init (fun r c -> getCell ((Shape.Height-1)-r,c))

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

[<StructuredFormatDisplay("{Width}x{Height}: {ShapeCounts}")>]
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

type RegionState = {
    Shapes: Map<int*int, Shape>;
    Cells: Set<int*int>;

} with
    member this.GetShapeSlot (row:int, col:int) =
        let getCell rowOffset colOffset =
            let cellPos = (row + rowOffset, col + colOffset)
            this.Cells.Contains cellPos
        Shape.Init getCell 

    member this.Item
        with get key = this.GetShapeSlot key

    member this.AddShape (row:int) (col:int) (shape:Shape) =
        let newCells =
            Shape.CellOffsets
            |> Seq.filter shape.GetCell
            |> Seq.map (addTuples (row, col))
            |> Set.ofSeq
        {
            Shapes = this.Shapes.Add ((row, col), shape)
            Cells = this.Cells + newCells
        }

    static member New = { Shapes = Map.empty; Cells = Set.empty }


let regionStateToCellStrings (region:Region) (regionState: RegionState) =
    let mutable i = 0
    let mutable cells = Array2D.create region.Height region.Width Shape.EmptySymbol
    let setCell (symbol:string) (row:int, col:int) = cells[row,col] <- symbol
    let setShapeCells (row:int, col:int) (shape:Shape) =
        let symbol = Shape.Symbols[i % Shape.Symbols.Length]
        i <- i + 1
        Shape.CellOffsets
        |> Seq.filter shape.GetCell
        |> Seq.map (addTuples (row, col))
        |> Seq.iter (setCell symbol)

    regionState.Shapes |> Map.iter setShapeCells
    cells

let regionStateToString (region:Region) (regionState: RegionState) =
    let cellStrings = regionStateToCellStrings region regionState
    seq {
        yield ("┌" + String('─',region.Width*2) + "┐\n")
        for row: int in 0..(region.Height - 1) do
            yield "│"
            for col in 0..(region.Width - 1) do
                yield cellStrings[row,col]
            yield "│\n"
        yield ("└" + String('─',region.Width*2) + "┘\n")
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

    let shapeTypeCounts = Array.copy region.ShapeTypeCounts
    let mutable regionState: RegionState = RegionState.New

    let addShape row col shape shapeType =
        regionState <- regionState.AddShape row col shape
        shapeTypeCounts[shapeType] <- shapeTypeCounts[shapeType] - 1
        regionStateToString region regionState
        |> Seq.iter (printf "%s")
        printfn "%A" shapeTypeCounts

    printfn "%A" shapeTypeCounts

    // Deliberately place the first shape in the top-left corner
    let firstShapeType = 
        shapeTypeCounts
        |> Array.mapi (fun i count -> (i, count))
        |> Array.filter (fun (_, count) -> count > 0)
        |> Array.head
        |> fst
    addShape 0 0 shapes[firstShapeType] firstShapeType

    for row in 0..(region.Height - Shape.Width) do
        for col in 01..(region.Width - Shape.Height) do
            let shapeSlot = regionState[row, col]
            printfn "Trying to fit shape at (%d,%d) into slot:\n%s" row col (shapeSlot.Format)
            let possibleShapes = shapeSlots.GetShapesThatFit shapeSlot
            
            let bestFitShapeType = 
                possibleShapes
                |> Array.choose getShapeType
                |> Array.filter (fun (shape2, shapeType) -> shapeTypeCounts[shapeType] > 0)
                |> Array.tryHead

            match bestFitShapeType with
            | Some (shape, shapeType) -> addShape row col shape shapeType
            | _ -> ()

    shapeTypeCounts


let result1 input = 
    let lines = File.ReadAllLines(input)
    let shapes = readShapes lines
    let regions = readRegions lines[30..]

    shapes |> Array.iteri (fun i shape -> printfn "%d:\n%s" i (shape.Format))

    regions
    |> Array.map (tryFitShapes shapes)


printfn "Part 1 Example: %A" (result1 "example.txt")
//printfn "Part 1 Result: %A" (result1 "input.txt")

