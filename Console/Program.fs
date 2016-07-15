open System
open System.IO

open Structure
open Solve
open Display
open Parse

let (|CellParse|_|) c = Cell.tryParse c

let constructGrid () =
    Console.Clear ()

    let origCol, origRow = Console.CursorLeft, Console.CursorTop
    let setCursor (Coordinate.Coordinate (x,y)) = Console.SetCursorPosition (origCol + x, origCol + y)

    let blank = Grid.init
    Grid.render blank |> Seq.iter Console.WriteLine

    Console.WriteLine ()
    Console.WriteLine "Welcome to Sudoku. Fill in the grid above using the arrow keys to navigate, the digits 1-9 to set a symbol, or delete/full stop to clear it. When finished, press Enter."

    Console.SetCursorPosition (origCol, origRow)

    let rec gridLoop g co =
        let move dir =
            let newPos = (Coordinate.add dir co)
            setCursor newPos
            gridLoop g newPos

        let set c =
            Console.Write (Cell.render c)
            setCursor co
            gridLoop (Grid.set co c g) co

        let key = Console.ReadKey true
        match key.Key, key.KeyChar with
        | ConsoleKey.Enter, _      -> g
        | ConsoleKey.UpArrow, _    -> move (0,-1)
        | ConsoleKey.DownArrow, _  -> move (0, 1)
        | ConsoleKey.LeftArrow, _  -> move (-1,0)
        | ConsoleKey.RightArrow, _ -> move ( 1,0)
        | _, CellParse c           -> set c
        | _                        -> gridLoop g co

    gridLoop blank Coordinate.zero

[<EntryPoint>]
let main argv = 
    let grid = constructGrid ()

    Console.Clear ()
    Grid.render grid |> Seq.iter Console.WriteLine
    Console.WriteLine()
    Console.WriteLine "Solving..."
    
    let solved = Grid.solve grid

    Console.Clear ()

    Grid.render solved |> Seq.iter Console.WriteLine

    Console.WriteLine()
    if Grid.isSolved solved then do
        Console.WriteLine "Hooray, solved!"
    else
        Console.WriteLine "Could not solve :("


    Console.ReadKey true
    0 
