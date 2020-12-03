module Trajectory

let traverse (expression : string[]) (right : int, down : int) =
    let map = expression
            |> Array.map (fun str -> str.ToCharArray())

    let xMax = map.[0].Length
    let yMax = expression.Length

    let rec move (x, y) trees =
        let cnt = if map.[y].[x] = '#' then trees + 1 else trees
        let newX = (x + right) % xMax
        let newY = y + down

        if newY >= yMax then
            cnt
        else
            move (newX, newY) cnt

    move (0, 0) 0

