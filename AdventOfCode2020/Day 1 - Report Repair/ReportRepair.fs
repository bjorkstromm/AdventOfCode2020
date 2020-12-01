module ReportRepair

let sumTo2020 entries =
    let rec test xs x =
        let l = xs |> List.length
        let head = xs |> List.head

        match (head + x), l with
        | 2020, _ -> Some (x, head)
        | _, 1 -> None
        | _, _ -> test (xs |> List.tail) x

    entries
    |> List.map (fun x -> test (List.except [x] entries) x)
    |> List.choose id
    |> List.head
