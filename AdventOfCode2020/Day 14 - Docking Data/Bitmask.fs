module Shuttle
open System
open System.Collections.Generic

type Mask = (int * bool) array
type Mem = int * int64

type Instruction =
    | Mask of Mask
    | Mem of Mem


let applyMask value mask =
    let rec apply arr (value : int64) =
        match arr with
        | [] -> value
        | (i, s)::tail ->
            if s then
                apply tail (value ||| (1L <<< i))
            else
                apply tail (value &&& ~~~(1L <<< i))
    let mask = mask |> Array.toList
    apply mask value

let apply (exp : string) =
    let instructions =
        exp.Split(Environment.NewLine)
        |> Array.map (fun str ->
            let tokens = str.Split(" = ")
            match tokens.[0] with
            | "mask" ->
                tokens.[1].ToCharArray()
                |> Array.rev
                |> Array.mapi (fun i c -> (i, c))
                |> Array.filter (fun (_, c) -> c <> 'X')
                |> Array.map (fun (i,c) -> (i, c = '1'))
                |> Mask
            | instr when instr.StartsWith("mem[") ->
                Mem (instr.Substring(4, instr.Length - 5) |> int, tokens.[1] |> int64)
            | instr -> failwithf "Invalid instruction %s" instr)


    let folder ((mem, mask) : (Dictionary<int, int64> * Mask)) (instruction : Instruction) =
        match instruction with
        | Mask newMask -> (mem, newMask)
        | Mem (address, value) ->
            mem.[address] <- (applyMask value mask)
            (mem, mask)


    let (mem, _) =
        instructions
        |> Array.fold folder (Dictionary<int, int64>(), [||])

    mem.Values |> Seq.sum