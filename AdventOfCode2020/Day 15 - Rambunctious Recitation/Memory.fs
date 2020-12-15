module Memory

let play (numbers : int array) (turns : int) =
    let tryFirstIndexOfRev num numbers =
        numbers
        |> Array.rev
        |> Array.tryFindIndex ((=) num)

    let next numbers =
        let num = numbers |> Array.last
        match tryFirstIndexOfRev num numbers.[..numbers.Length-2] with
        | None -> 0
        | Some i -> i + 1

    let rec loop (numbers : int array) =
        if numbers.Length >= turns then
            numbers
        else
            let num = next numbers
            loop (Array.append numbers [|num|])

    loop numbers



