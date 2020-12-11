module Adapters
open System.Collections.Generic

let joltage (adapters : int array) =
    let rec count (x, y, z) arr =
        match arr with
        | [] -> (x, y, z)
        | head::tail ->
            match (tail |> List.tryHead) with
            | None -> (x, y, z)
            | Some n ->
                match n-head with
                | 1 -> count (x+1, y, z) tail
                | 2 -> count (x, y+1, z) tail
                | 3 -> count (x, y, z+1) tail
                | diff -> failwithf "Invalid difference %i" diff

    adapters
    |> Array.sort
    |> Array.toList
    |> count (1,1,1)

// Solution below borrowed from CompositionalIT
// https://github.com/CompositionalIT/advent-of-code-2020/blob/6aedcf360cc400ecb58c5ae55317d6c916b62376/Solutions.fsx#L394-L413

let combinations (adapters : int array) =
    let adapters = adapters
                   |> Array.append [|0; (Array.max adapters)+3|]
                   |> Array.sort

    let nextLookup =
        adapters
        |> Array.map(fun a ->
            let p = adapters |> Array.filter(fun r -> r > a && r <= a + 3)
            (a, p))
        |> Map

    let rec calculateSize (nextLookup : Map<_,_>) (cache : IDictionary<_,_>) current =
        match nextLookup.[current] with
        | [||] -> 1UL
        | nextNumbers ->
            nextNumbers
            |> Array.sumBy (fun number ->
                if not (cache.ContainsKey number) then
                    cache.Add (number, calculateSize nextLookup cache number)
                cache.[number])


    calculateSize nextLookup (Dictionary()) adapters.[0]