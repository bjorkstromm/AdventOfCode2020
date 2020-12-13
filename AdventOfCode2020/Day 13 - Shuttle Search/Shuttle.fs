module Shuttle

open System
    
let search (exp : string array) =
    let earliest = exp.[0] |> int
    let busIds = exp.[1].Split(",")
                 |> Array.map (fun str ->
                     match Int32.TryParse str with
                     | true, num -> Some num
                     | _ -> None)
                 |> Array.choose id

    let ordered = busIds
                    |> Array.map (fun id ->
                        let time = earliest - (earliest % id) + id
                        (id, time - earliest))
                    |> Array.sortBy (fun (_, time) -> time)
    
    ordered |> Array.head
