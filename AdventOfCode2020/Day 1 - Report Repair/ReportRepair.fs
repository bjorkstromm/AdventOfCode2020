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

let sumTo2020part2 entries =
    let rec test (x, y, xs) =
        let head = xs |> List.head

        match (head + x + y, xs |> List.tail) with
        | (2020, _) -> Some (x, y, head)
        | (_, []) -> None
        | (_, tail) -> test (x, y, tail)

    let rec test2 (x, xs) =
        let head = xs |> List.head
        let r = test (x, head, entries |> List.except [x; head])

        match (r, xs |> List.tail) with
        | (Some r, _) -> Some r
        | (_, []) -> None
        | (_, tail) -> test2 (x, tail)

    entries
    |> List.map (fun x -> (x, entries |> List.except [x]))
    |> List.map test2
    |> List.choose id
    |> List.head