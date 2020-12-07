module Rules

open System
open System.Text.RegularExpressions

let RegexMatches (pattern : string) (input : string) =
    Regex.Matches(input, pattern)
    |> Seq.map (fun m -> m.Groups |> Seq.last)
    |> Seq.map (fun g -> g.Value)
    |> Seq.toArray

let processRules (ruleExp : string) (target : string) =
    let parseRule (ruleExp : string) =
        let tokens = ruleExp.Split(" bags contain ")
        let bag = tokens |> Array.head
        let contains = RegexMatches @"\d+\s(.*?)\sbag" (tokens |> Array.last)
        (bag, contains)

    let parseRules (ruleExp : string) =
        ruleExp.Split(Environment.NewLine)
        |> Array.map parseRule
        |> Map.ofArray

    let rules = parseRules ruleExp

    let rec contains (bag : string) =
        let c = rules.[bag]
        match c |> Array.contains target with
        | true -> true
        | _ -> c |> Array.exists contains

    rules
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.filter contains
    |> Seq.toArray
