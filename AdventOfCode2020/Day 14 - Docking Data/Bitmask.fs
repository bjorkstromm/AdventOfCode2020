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

// Part 2

type Mem2 = int64 * int64

type Instruction2 =
    | Mask of string
    | Mem of Mem2

let applyMask2 address (mask : string) =
    let rec intToBinary i =
        match i with
        | 0 | 1 -> string i
        | _ ->
            let bit = string (i % 2)
            (intToBinary (i / 2)) + bit

    let folder (mask : char array) (c : char) =
        let index = mask |> Array.findIndex ((=) 'X')
        Array.concat [| mask.[..index-1] ; [|c|] ; mask.[index+1..] |]

    let combinations = mask |> Seq.filter ((=) 'X') |> Seq.length

    // TODO: Apply to address first.

    [|0..combinations-1|]
    |> Array.map (fun i ->
        let arr = (intToBinary i).ToCharArray()
        arr |> Array.fold folder (mask.ToCharArray()))
    |> Array.map string
    |> Array.map (fun str -> System.Convert.ToInt64(str, 2))

let apply2 (exp : string) =
    let instructions =
        exp.Split(Environment.NewLine)
        |> Array.map (fun str ->
            let tokens = str.Split(" = ")
            match tokens.[0] with
            | "mask" -> Mask tokens.[1]
            | instr when instr.StartsWith("mem[") ->
                Mem (instr.Substring(4, instr.Length - 5) |> int64, tokens.[1] |> int64)
            | instr -> failwithf "Invalid instruction %s" instr)

    let folder ((mem, mask) : (Dictionary<int64, int64> * string)) (instruction : Instruction2) =
        match instruction with
        | Mask newMask -> (mem, newMask)
        | Mem (address, value) ->
            let addresses = applyMask2 address mask 
            addresses |> Array.iter (fun i -> mem.[i] <- value)
            (mem, mask)

    let (mem, _) =
        instructions
        |> Array.fold folder (Dictionary<int64, int64>(), "")

    mem.Values |> Seq.sum