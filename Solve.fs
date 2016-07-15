module Solve

open Structure
open Cell

module Action =
    type Action = Elimination of Coordinate.Coordinate * Symbol

    let apply a g =
        match a with
        | Elimination (co, s) ->
            match Grid.get co g |> eliminate s with
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
            | [_, Certain elim; co, Possible xs]
            | [co, Possible xs; _, Certain elim]
                when Set.contains elim xs
                -> Action.Elimination (co, elim) |> Seq.singleton
            | [co1, Possible xs1; co2, Possible xs2]
                when Set.count xs1 = 2 && xs1 = xs2
                -> seq {
                       for x in xs1 do
                       for co, cell in g |> Map.toSeq do
                       if co <> co1 && co <> co2 && Cell.hasPossible x cell then
                           yield Action.Elimination (co, x)
                   }
            | _ -> Seq.empty
        
        g
        |> Map.toList
        |> comb 2
        |> Seq.collect actionForCells
            

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
        