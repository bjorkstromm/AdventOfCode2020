module XMAS

let processXMAS (numbers : int64 array) (preambleLength : int) =

    let rec processNum (arr : int64 array) =
        if preambleLength >= arr.Length then
            failwith "Preamble must be smaller than length of array."
        else
            let preamble = arr.[0..preambleLength]
            let value = arr.[preambleLength]
            let rec check i =
                if i = preambleLength then
                    false
                else
                    let x = preamble.[i] 
                    let arr = match i with
                              | 0 -> preamble |> Array.tail
                              | _ -> Array.concat [| preamble.[..i-1] ; preamble.[i+1..] |]
                    match arr |> Array.tryFind (fun y -> x + y = value) with
                    | None -> check (i+1)
                    | Some _ -> true

            match check 0 with
            | false -> value
            | true -> arr |> Array.tail |> processNum 

    numbers |> processNum

let processXMASv2 (numbers : int64 array) (preambleLength : int) =

    let num = processXMAS numbers preambleLength

    let rec findSet numbers =
        let rec trySum arr set =
            let head = arr |> Array.head
            let set = head::set
            match set |> List.sum with
            | sum when sum < num -> trySum (arr |> Array.tail) set
            | sum when sum > num -> None
            | _ -> Some set

        match trySum numbers [] with
        | None -> numbers |> Array.tail |> findSet
        | Some set -> set

    let set = numbers |> findSet |> List.sort
    let min = set |> List.head
    let max = set |> List.last

    min + max