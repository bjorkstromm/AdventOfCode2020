module Cubes
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Collections.ParallelSeq

type State =
    | Active
    | Inactive

let scan (exp : string []) (cycles : int) =
    let nearby (x,y,z) =
        [|x-1..x+1|]
        |> Array.collect (fun x ->
            [|y-1..y+1|]
            |> Array.collect (fun y ->
                [|z-1..z+1|]
                |> Array.map (fun z -> (x,y,z))))
        |> Array.except [|(x,y,z)|]

    let initial = 
        exp
        |> Array.mapi (fun y str ->
            str.ToCharArray()
            |> Array.mapi (fun x c ->
                let state = if c = '#' then Active else Inactive
                ((x, y, 0), state)))
        |> Array.collect id
        |> dict

    let rec loop (map : IDictionary<_,_>) (cycle : int) =
        let dict = ConcurrentDictionary<(int*int*int),State>()
        map.Keys
        |> PSeq.collect nearby
        |> PSeq.iter (fun current ->
            let activeNearby =
                current
                |> nearby
                |> Array.map (fun n ->
                    match map.TryGetValue(n) with
                    | (false, _) -> Inactive
                    | (true, s) -> s
                )
                |> Array.filter (fun s -> s = Active)
                |> Array.length

            let currentState =
                match map.TryGetValue(current) with
                | (false, _) -> Inactive
                | (true, s) -> s

            match (currentState, activeNearby) with
            | (Active, 2) | (Active, 3) -> dict.[current] <- Active
            | (Active, _) -> dict.[current] <- Inactive
            | (Inactive, 3) -> dict.[current] <- Active
            | (Inactive, _) -> dict.[current] <- Inactive
        )

        match cycle + 1 with
        | c when c = cycles -> dict
        | c -> loop dict c


    let result = loop initial 0

    result.Values
    |> Seq.filter (fun s -> s = Active)
    |> Seq.length

let scan2 (exp : string []) (cycles : int) =
    let nearbyCache = ConcurrentDictionary<(int*int*int*int),(int*int*int*int)[]>()

    let nearby (x,y,z,w) =
        nearbyCache.GetOrAdd((x,y,z,w), fun (x,y,z,w) ->
            [|x-1..x+1|]
            |> Array.collect (fun x ->
                [|y-1..y+1|]
                |> Array.collect (fun y ->
                    [|z-1..z+1|]
                    |> Array.collect (fun z ->
                        [|w-1..w+1|]
                        |> Array.map (fun w -> (x,y,z,w)))))
            |> Array.except [|(x,y,z,w)|])

    let initial = 
        exp
        |> Array.mapi (fun y str ->
            str.ToCharArray()
            |> Array.mapi (fun x c ->
                let state = if c = '#' then Active else Inactive
                ((x, y, 0, 0), state)))
        |> Array.collect id
        |> dict

    let rec loop (map : IDictionary<_,_>) (cycle : int) =
        let dict = ConcurrentDictionary<(int*int*int*int),State>()
        map.Keys
        |> PSeq.collect nearby
        |> PSeq.iter (fun current ->
            let activeNearby =
                current
                |> nearby
                |> Array.map (fun n ->
                    match map.TryGetValue(n) with
                    | (false, _) -> Inactive
                    | (true, s) -> s
                )
                |> Array.filter (fun s -> s = Active)
                |> Array.length

            let currentState =
                match map.TryGetValue(current) with
                | (false, _) -> Inactive
                | (true, s) -> s

            match (currentState, activeNearby) with
            | (Active, 2) | (Active, 3) -> dict.[current] <- Active
            | (Active, _) -> dict.[current] <- Inactive
            | (Inactive, 3) -> dict.[current] <- Active
            | (Inactive, _) -> dict.[current] <- Inactive
        )

        match cycle + 1 with
        | c when c = cycles -> dict
        | c -> loop dict c


    let result = loop initial 0

    result.Values
    |> Seq.filter (fun s -> s = Active)
    |> Seq.length