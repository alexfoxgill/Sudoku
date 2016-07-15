module Display

open Structure

module Cell =
    let render = function
    | Cell.Certain c ->
        let i = Array.findIndex (fun x -> x = c) Symbol.all
        i + (int '1') |> char
    | Cell.Possible xs -> '.'


module Grid =
    let render g =
        let max = snd bounds
        Coordinate.all
        |> Seq.map (fun c -> Grid.get c g |> Cell.render)
        |> Seq.chunkBySize (max + 1)
        |> Seq.map (fun cs -> new System.String(cs))