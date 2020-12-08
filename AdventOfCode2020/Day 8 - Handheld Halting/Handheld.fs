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

let fixAndExecute (bootCode : string) =
    let rec run (acc, index, executed, instructions) =
        let max = instructions |> Array.length
        match index with
        | i when executed |> List.contains i -> None
        | i when i < 0 -> None
        | i when i > max -> None
        | i when i = max -> Some acc
        | _ -> match (instructions.[index], index + 1, index :: executed) with
               | (("nop", _), i, e) -> run (acc, i, e, instructions)
               | (("acc", a), i, e) -> run (acc + a, i, e, instructions)
               | (("jmp", a), _, e) -> run (acc, index + a, e, instructions)
               | _ -> None

    let instructions = bootCode.Split(Environment.NewLine)
                       |> Array.map (fun str -> str.Split(" "))
                       |> Array.map (fun arr -> (arr |> Array.head, arr |> Array.last |> int))
    let max = instructions |> Array.length

    let opsToFix = instructions
                   |> Array.mapi ( fun i (o, a) -> match o with
                                                   | "nop" -> Some (i, ("jmp", a))
                                                   | "jmp" -> Some (i, ("nop", a))
                                                   | _ -> None)
                   |> Array.choose id

    let rec fixAndRun arr =
        let swap (i, op) =
            match i with
            | 0 -> Array.append [|op|] instructions.[1..]
            | n when n = (max - 1) -> Array.append instructions.[0..max-2] [|op|]
            | n -> Array.concat [| instructions.[0..n-1] ; [|op|] ; instructions.[n+1..] |]

        let fix = arr |> Array.head |> swap

        match run (0, 0, [], fix) with
        | Some x -> x
        | None -> fixAndRun (arr |> Array.tail)

    fixAndRun opsToFix
