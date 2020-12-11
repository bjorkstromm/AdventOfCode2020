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

type Direction =
    | NW
    | N
    | NE
    | W
    | E
    | SW
    | S
    | SE

let allDirections = [|
    NW
    N
    NE
    W
    E
    SW
    S
    SE
|]

let simulate2 (exp : string) =
    let occupiedNearby (x,y) (map :char [,]) =
        let xLen = map |> Array2D.length2
        let yLen = map |> Array2D.length1
        let rec findNearby (x,y) direction =
            let (newX, newY) =
                match direction with
                | NW -> (x-1, y-1)
                | N -> (x, y-1)
                | NE -> (x+1,y-1)
                | W -> (x-1, y)
                | E -> (x+1, y)
                | SW -> (x-1, y+1)
                | S -> (x, y+1)
                | SE -> (x+1, y+1)
            match (newX, newY) with
            | (-1, _) -> '.'
            | (_, -1) -> '.'
            | (n, _) when n = xLen -> '.'
            | (_, n) when n = yLen -> '.'
            | _ ->
                match map.[newY,newX] with
                | '.' -> findNearby (newX, newY) direction
                | c -> c
        allDirections
        |> Array.map (fun dir -> findNearby (x,y) dir)
        |> Array.filter (fun c -> c = '#')
        |> Array.length

    let getState (x,y) (map : char [,]) =
        match map.[y,x] with
        | c when c = 'L' || c = '#' ->
            match (c, occupiedNearby (x,y) map) with
            | ('L', 0) -> '#'
            | ('#', n) when n >= 5 -> 'L'
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