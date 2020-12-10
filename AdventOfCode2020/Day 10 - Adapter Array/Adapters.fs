module Adapters

let joltage (adapters : int array) =
    let rec count (x, y, z) arr =
        match arr with
        | [] -> (x, y, z)
        | head::tail ->
            match (tail |> List.tryHead) with
            | None -> (x, y, z)
            | Some n ->
                match n-head with
                | 1 -> count (x+1, y, z) tail
                | 2 -> count (x, y+1, z) tail
                | 3 -> count (x, y, z+1) tail
                | diff -> failwithf "Invalid difference %i" diff

    adapters
    |> Array.sort
    |> Array.toList
    |> count (1,1,1)