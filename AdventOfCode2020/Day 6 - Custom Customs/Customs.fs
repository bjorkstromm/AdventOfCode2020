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

let scan2 (input : string) =
    let scanPerson (p : string) =
        p.ToCharArray()
        |> Set.ofArray

    let scanGroup (g : string) =
        g.Split(Environment.NewLine)
        |> Array.map scanPerson
        |> Set.intersectMany
        |> Set.toArray

    input.Split(Environment.NewLine + Environment.NewLine)
    |> Array.map scanGroup
    |> Array.map Array.length