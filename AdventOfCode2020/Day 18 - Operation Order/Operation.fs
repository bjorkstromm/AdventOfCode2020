module Operation

open System

let calc (exp : string) =
    let rec loop state op lst =
        match lst with
        | [] -> (state, [])
        | char::tail ->
            match (char, Int64.TryParse(char.ToString())) with
            | (' ', _) -> tail |> loop state op
            | ('+', _) -> tail |> loop state (+)
            | ('*', _) -> tail |> loop state (*)
            | ('(', _) ->
                let (inner, lst) = tail |> loop 0L (+)
                lst |> loop (state |> op inner) op
            | (')', _) -> (state, tail)
            | (_, (true, n)) -> tail |> loop (state |> op n) op
            | (c, (_,_)) -> failwithf "Invalid character %c" c

    let (sum, _) =
        exp.ToCharArray()
        |> Array.toList
        |> loop 0L (+)

    sum
