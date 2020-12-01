module ReportRepair

let sumTo2020 entries =
    let rec test (x, xs) =
        let head = xs |> List.head

        match (head + x, xs |> List.tail) with
        | (2020, _) -> Some (x, head)
        | (_, []) -> None
        | (_, tail) -> test (x, tail)

    entries
    |> List.map (fun x -> (x, entries |> List.except [x]))
    |> List.map test
    |> List.choose id
    |> List.head