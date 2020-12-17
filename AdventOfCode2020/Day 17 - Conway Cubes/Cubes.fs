module Cubes
open System.Collections.Generic

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
        // TODO: Should be ConcurrentDictionary
        let dict = Dictionary<(int*int*int),State>()
        map.Keys
        |> Seq.toArray
        |> Array.collect nearby
        |> Array.iter (fun current -> // TODO: Should be Array.Parallel.iter
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