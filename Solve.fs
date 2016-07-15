module Solve

open Structure

module Action =
    type Action = Elimination of Coordinate.Coordinate * Symbol

    let apply a g =
        match a with
        | Elimination (co, s) ->
            match Grid.get co g |> Cell.eliminate s with
            | Some v -> Grid.set co v g
            | _ -> g


module Group =
    let getActions (Group.Group g) =
        let rec comb n l =
            match (n,l) with
            | (0,_) -> [[]]
            | (_,[]) -> []
            | (n,x::xs) ->
                let useX = List.map (fun l -> x::l) (comb (n-1) xs)
                let noX = comb n xs
                useX @ noX

        let actionForCells =
            function
            | [_, Cell.Certain elim; co, Cell.Possible xs]
            | [co, Cell.Possible xs; _, Cell.Certain elim]
                when Set.contains elim xs -> Action.Elimination (co, elim) |> Some
            | _ -> None
        
        g
        |> Map.toList
        |> comb 2
        |> Seq.choose actionForCells
            

module Grid =
    let rec solve g =
        let actions =
            seq {
                let min,max = bounds
                for x in min..max do
                for f in [Grid.getRow; Grid.getCol; Grid.getSqr] do
                let group = f x g
                match f x g with
                | Some group -> yield! Group.getActions group
                | None -> ()
            } |> Seq.toList

        if List.isEmpty actions then g
        else
            List.fold (fun g a -> Action.apply a g) g actions
            |> solve
        