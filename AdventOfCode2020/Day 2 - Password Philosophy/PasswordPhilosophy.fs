module PasswordPhilosophy

type PasswordAndPolicy = {
    Char: char
    Min: int
    Max: int
    Password: string
    Expression: string
}

let validate entries =
    let isValid pw =
        let occurrences = pw.Password.ToCharArray()
                        |> Array.filter (fun x -> x = pw.Char)
                        |> Array.length

        match occurrences with
        | x when pw.Min <= x && x <= pw.Max -> true
        | _ -> false

    let parse (expression : string) =
        let parseMinMax (str : string) =
            let tokens = str.Split('-')
            match tokens.Length with
            | 2 -> (tokens.[0] |> int, tokens.[1] |> int)
            | _ -> (-1, -1)

        let tokens = expression.Split(' ')

        if tokens.Length = 3 then
            let (min, max) = tokens.[0] |> parseMinMax
            let char = tokens.[1].ToCharArray().[0]
            let password = tokens.[2]

            Some {
                Char = char
                Min = min
                Max = max
                Password = password
                Expression = expression
            }
        else
            None

    let validPasswords = entries
                        |> List.map parse
                        |> List.choose id
                        |> List.filter isValid
                        |> List.map (fun x -> x.Expression)

    (validPasswords, entries |> List.except validPasswords)
