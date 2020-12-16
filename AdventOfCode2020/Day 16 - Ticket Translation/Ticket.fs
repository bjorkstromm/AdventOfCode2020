module Tickets

type Range = {
    Low: int
    High: int
}

type Rule = {
    Name: string
    Ranges: Range array
}

type Ticket = int array

type Notes = {
    Rules: Rule list
    MyTicket: Ticket
    NearbyTickets: Ticket list
}

type State =
    | Rules
    | MyTicket
    | NearbyTickets

let scan (exp : string) =
    let parse (exp : string) : Notes =
        let rec loop (notes : Notes) (state : State) (rows : string list) =
            match rows with
            | [] -> notes
            | row::tail ->
                match (state, row) with
                | (_, "") -> loop notes state tail
                | (_, "your ticket:") -> loop notes MyTicket tail
                | (_, "nearby tickets:") -> loop notes NearbyTickets tail
                | (MyTicket, row) ->
                    loop {
                        notes with MyTicket = row.Split(",") |> Array.map int
                    } state tail
                | (NearbyTickets, row) ->
                    loop {
                        notes with NearbyTickets = (row.Split(",") |> Array.map int)::notes.NearbyTickets
                    } state tail
                | (Rules, row) ->
                    let tokens = row.Split(":")
                    let name = tokens.[0]
                    let ranges =
                        tokens.[1].Split(" or ")
                        |> Array.map (fun str ->
                            let bounds = str.Split("-") |> Array.map int
                            { 
                                Low = bounds.[0]
                                High = bounds.[1]
                            })
                    let rule = { 
                        Name = name
                        Ranges = ranges
                    }
                    loop {
                        notes with Rules = rule::notes.Rules
                    } state tail

        loop { 
            Rules = []
            MyTicket = [||]
            NearbyTickets = []
        } Rules (exp.Split(System.Environment.NewLine) |> List.ofArray)

    let notes = parse exp
    let ranges =
        notes.Rules
        |> List.collect (fun rule -> rule.Ranges |> List.ofArray)
        |> List.distinct

    let checkValidity (state : int list) (ticket : Ticket) : int list =
        let isInRange (num : int ) (range : Range) : bool =
            range.Low <= num && num <= range.High

        let isValid (num : int) =
            ranges
            |> List.toSeq
            |> Seq.exists (fun r -> isInRange num r)

        let invalid =
            ticket
            |> Array.filter (isValid >> not)
            |> Array.toList

        List.append invalid state

    let invalid =
        notes.NearbyTickets
        |> List.fold checkValidity []

    invalid
    |> List.sum

