module BoardingPass

type BoardingPass = {
    Row: int
    Column: int
    Id: int
}

let scan (input : string[]) =
    let rows = [|0..127|]
    let columns = [|0..7|]

    let upper a = a |> Array.splitInto 2 |> Array.last
    let lower a = a |> Array.splitInto 2 |> Array.head

    let rec getRow r xs =
        match xs |> Array.tryHead with
        | None -> r |> Array.head
        | Some 'B' -> getRow (r |> upper) (xs |> Array.tail)
        | Some 'F' -> getRow (r |> lower) (xs |> Array.tail)
        | _ -> failwith "Invalid row"

    let rec getColumn c xs =
        match xs |> Array.tryHead with
        | None -> c |> Array.head
        | Some 'R' -> getColumn (c |> upper) (xs |> Array.tail)
        | Some 'L' -> getColumn (c |> lower) (xs |> Array.tail)
        | _ -> failwith "Invalid column"

    input
    |> Array.map (fun x -> x.ToCharArray())
    |> Array.map (fun x -> Array.splitAt 7 x)
    |> Array.map (fun (r, c) -> (getRow rows r, getColumn columns c))
    |> Array.map (fun (r, c) -> {
        Row = r
        Column = c
        Id = (r * 8) + c
    })
