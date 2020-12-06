module Customs

open System

let scan (input : string) =
    let scanPerson (p : string) =
        p.ToCharArray()

    let scanGroup (g : string) =
        g.Split(Environment.NewLine)
        |> Array.map scanPerson
        |> Array.concat
        |> Array.distinct

    input.Split(Environment.NewLine + Environment.NewLine)
    |> Array.map scanGroup
    |> Array.map Array.length