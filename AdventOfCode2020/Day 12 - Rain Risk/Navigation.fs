module Navigation

type Direction = 
    | North
    | West
    | East
    | South

type Pos = {
    Direction: Direction
    X: int
    Y: int
}

let navigate (exp : string) =
    let rec move (pos : Pos) (instruction : (char * int)) =
        let getDirection dir degrees =
            let dirs = [|North;East;South;West|]
            let idx = dirs |> Array.findIndex (fun d -> d = dir)
            let i = match ((idx+(degrees / 90)) % 4) with
                    | i when i < 0 -> 4 + i
                    | i -> i
            dirs.[i]

        match instruction with
        | ('N', y) -> { pos with Y = pos.Y-y }
        | ('S', y) -> { pos with Y = pos.Y+y }
        | ('E', x) -> { pos with X = pos.X+x }
        | ('W', x) -> { pos with X = pos.X-x }
        | ('L', deg) -> { pos with Direction = getDirection pos.Direction -deg}
        | ('R', deg) -> { pos with Direction = getDirection pos.Direction deg }
        | ('F', n) -> match pos.Direction with
                      | North -> move pos ('N', n)
                      | West -> move pos ('W', n)
                      | East -> move pos ('E', n)
                      | South -> move pos ('S', n)
        | (c, _) -> failwithf "Invalid action %c" c

    let instructions = 
        exp.Split(System.Environment.NewLine)
        |> Array.map (fun str -> str.[0], str.[1..] |> int)

    let pos =
        instructions
        |> Array.fold move { X = 0; Y = 0; Direction = East }

    (abs pos.X) + (abs pos.Y)

type Position = {
    X: int
    Y: int
}

type Ship = {
    Waypoint: Position
    Position: Position
}

let navigate2 (exp : string) =
    let rec move (ship : Ship) (instruction : (char * int)) =
        let rotate wp deg =
            match (deg + 360) % 360 with
            | 0 -> wp
            | 90 -> { Y = wp.X; X = -wp.Y }
            | 180 -> { Y = -wp.Y; X = -wp.X }
            | 270 -> { Y = -wp.X; X = wp.Y }
            | _ -> failwithf "Invalid degrees %i" deg

        match instruction with
        | ('N', y) -> { ship with Waypoint = { ship.Waypoint with Y = ship.Waypoint.Y - y } }
        | ('S', y) -> { ship with Waypoint = { ship.Waypoint with Y = ship.Waypoint.Y + y } }
        | ('E', x) -> { ship with Waypoint = { ship.Waypoint with X = ship.Waypoint.X + x } }
        | ('W', x) -> { ship with Waypoint = { ship.Waypoint with X = ship.Waypoint.X - x } }
        | ('L', deg) -> { ship with Waypoint = rotate ship.Waypoint -deg }
        | ('R', deg) -> { ship with Waypoint = rotate ship.Waypoint deg }
        | ('F', n) -> { ship with Position = { X = ship.Position.X + (n * ship.Waypoint.X); Y = ship.Position.Y + (n * ship.Waypoint.Y) }}
        | (c, _) -> failwithf "Invalid action %c" c

    let instructions = 
        exp.Split(System.Environment.NewLine)
        |> Array.map (fun str -> str.[0], str.[1..] |> int)

    let ship =
        instructions
        |> Array.fold move { 
            Position = { X = 0; Y = 0 }
            Waypoint = { X = 10; Y = -1 }
        }

    (abs ship.Position.X) + (abs ship.Position.Y)