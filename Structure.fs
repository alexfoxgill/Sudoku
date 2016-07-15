module Structure

open System

let bounds = 0,8
let (>=<) x (min, max) = min <= x && x <= max
let clamp (x: int) (min, max) = Math.Max(Math.Min(x, max), min)

type Symbol = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    with
    static member all = [| One; Two; Three; Four; Five; Six; Seven; Eight; Nine |]


module Cell =
    type Cell =
        private
        | Certain of Symbol
        | Possible of Set<Symbol>

    let init = set Symbol.all |> Possible
    let certain = Certain

    let create xs =
        match Set.count xs with
        | 0 -> None
        | 1 -> xs |> Set.toSeq |> Seq.head |> Certain |> Some
        | _ -> Possible xs |> Some

    let eliminate x = function
        | Possible xs -> Set.remove x xs |> create
        | i -> i |> Some

    let (|Certain|Possible|) = function
    | Certain x -> Certain x
    | Possible xs -> Possible xs

    let isCertain = function
    | Certain _ -> true
    | _ -> false


module Coordinate =
    type Coordinate = private Coordinate of int * int
    let (|Coordinate|) = function Coordinate (x,y) -> x,y
    let zero = Coordinate (0,0)
    let all =
        let min,max = bounds
        seq { for y in min..max do for x in min..max do yield Coordinate (x,y) }

    let add (dx,dy) (Coordinate (x,y)) = (clamp (x+dx) bounds, clamp (y+dy) bounds) |> Coordinate
    let create (x,y) = if x >=< bounds && y >=< bounds then Coordinate (x,y)|> Some else None
    let row (Coordinate (x,y)) = x
    let column (Coordinate (x,y)) = y
    let square (Coordinate (x,y)) = (x/3)+(3*(y/3))
    
module Group =
    type Group = Group of Map<Coordinate.Coordinate, Cell.Cell>

module Grid =
    type Grid = private Grid of Map<Coordinate.Coordinate, Cell.Cell>

    let private apply f (Grid g) = g |> f |> Grid
    let init =
        Coordinate.all
        |> Seq.map (fun co -> co, Cell.init)
        |> Map.ofSeq
        |> Grid

    let get co (Grid g) = Map.find co g
    let set co v g = apply (Map.add co v) g

    let create xs = Seq.fold (fun g (co,v) -> set co v g) init

    let private getGroup groupf n (Grid g) =
        if (n >=< bounds)
        then g |> Map.filter (fun co _ -> groupf co = n) |> Group.Group |> Some
        else None

    let getRow = getGroup Coordinate.row
    let getCol = getGroup Coordinate.column
    let getSqr = getGroup Coordinate.square

    let isSolved (Grid g) = g |> Map.forall (fun _ c -> Cell.isCertain c)