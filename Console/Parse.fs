module Parse

open Structure

module Cell =
    let tryParse = function
    | x when x >=< ('1', '9') -> (int x) - (int '1') |> Array.get Symbol.all |> Cell.certain |> Some
    | '.' | ' ' | '?' -> Cell.init |> Some
    | _ -> None


module Grid =
    let tryParse s =
        let max = snd bounds
        s
        |> Seq.choose Cell.tryParse
        |> Seq.mapi (fun i c ->
            let co = Coordinate.create (i % max, i / max)
            match co, c with
            | Some co, Cell.Certain _ -> Some (co, c)
            | _ -> None)
        |> Seq.choose id
        |> Grid.create