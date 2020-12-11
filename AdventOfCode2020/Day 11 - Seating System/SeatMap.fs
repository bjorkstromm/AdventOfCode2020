module SeatMap

let simulate (exp : string) =
    let occupiedAdjacents (x,y) (map :char [,]) =
        let xMax = (map |> Array2D.length2)-1
        let yMax = (map |> Array2D.length1)-1

        let xStart = if x = 0 then x else x - 1
        let xEnd = if x = xMax then x else x + 1
        let yStart = if y = 0 then y else y - 1
        let yEnd = if y = yMax then y else y + 1

        [|xStart..xEnd|]
        |> Array.collect (fun x ->
            [|yStart..yEnd|]
            |> Array.map (fun y -> (x, y)))
        |> Array.filter (fun pos -> pos <> (x,y))
        |> Array.filter (fun (x, y) -> map.[y,x] = '#')
        |> Array.length

    let getState (x,y) (map : char [,]) =
        match map.[y,x] with
        | c when c = 'L' || c = '#' ->
            match (c, occupiedAdjacents (x,y) map) with
            | ('L', 0) -> '#'
            | ('#', n) when n >= 4 -> 'L'
            | (c, _) -> c
        | c -> c

    let rec sim (currentMap : char [,]) =
        let newMap = Array2D.copy currentMap
        let xLen = newMap |> Array2D.length2
        let yLen = newMap |> Array2D.length1

        [|0..xLen-1|]
        |> Array.Parallel.iter (fun x ->
            [|0..yLen-1|] |> Array.Parallel.iter (fun y ->
                 newMap.[y,x] <- getState (x,y) currentMap))

        match newMap = currentMap with
        | false -> sim newMap
        | _ ->
            newMap
            |> Seq.cast<char>
            |> Seq.toArray
            |> Seq.filter (fun c -> c = '#')
            |> Seq.length

    exp.Split(System.Environment.NewLine)
    |> Array.map (fun s -> s.ToCharArray())
    |> array2D
    |> sim