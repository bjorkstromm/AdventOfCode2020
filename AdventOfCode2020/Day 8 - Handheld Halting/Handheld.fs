module Handheld

open System

let execute (bootCode : string) =
    let instructions = bootCode.Split(Environment.NewLine)
                       |> Array.map (fun str -> str.Split(" "))
                       |> Array.map (fun arr -> (arr |> Array.head, arr |> Array.last |> int))

    let rec run (acc, index, executed) =
        if executed |> List.contains index then
            acc
        else
            match (instructions.[index], index + 1, index :: executed) with
            | (("nop", _), i, e) -> run (acc, i, e)
            | (("acc", a), i, e) -> run (acc + a, i, e)
            | (("jmp", a), _, e) -> run (acc, index + a, e)
            | _ -> failwith "Invalid operation"

    run (0, 0, [])
