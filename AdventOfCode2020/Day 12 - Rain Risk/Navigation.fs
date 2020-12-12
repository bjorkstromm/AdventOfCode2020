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