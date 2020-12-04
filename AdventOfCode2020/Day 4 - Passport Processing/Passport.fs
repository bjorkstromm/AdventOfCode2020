module Passport

type Passport = {
    BirthYear: string option
    IssueYear: string option
    ExpirationYear: string option
    Height: string option
    HairColor: string option
    EyeColor: string option
    PassportId: string option
    CountryId: string option
}

let processData (data : string) =
    let parsePassport (data : string) =
        let map = data.Replace(" ", System.Environment.NewLine).Split(System.Environment.NewLine)
                |> Array.map (fun kv -> match kv.Split(":") with
                                        | [|key;value|] -> Some (key, value)
                                        | _ -> None)
                |> Array.choose id
                |> Map.ofArray
        {
            BirthYear = map.TryFind("byr")
            IssueYear = map.TryFind("iyr")
            ExpirationYear = map.TryFind("eyr")
            Height = map.TryFind("hgt")
            HairColor = map.TryFind("hcl")
            EyeColor = map.TryFind("ecl")
            PassportId = map.TryFind("pid")
            CountryId = map.TryFind("cid")
        }

    let isValid passport =
        match passport with
        | { BirthYear = None } -> false
        | { IssueYear = None } -> false
        | { ExpirationYear = None } -> false
        | { Height = None } -> false
        | { HairColor = None } -> false
        | { EyeColor = None } -> false
        | { PassportId = None } -> false
        | _ -> true // We allow empty CountryId

    data.Split((System.Environment.NewLine + System.Environment.NewLine))
    |> Array.map parsePassport
    |> Array.filter isValid

let processDataV2 (data : string) =
    let validateRegex regex opt =
        match opt with
        | Some str -> let m = System.Text.RegularExpressions.Regex(regex).Match(str)
                      if m.Success then Some str else None
        | _ -> None

    let validateIntBetween min max opt =
        match opt |> validateRegex @"^\d+$" with
        | Some x -> match x |> int with
                    | x when min <= x && x <= max -> opt
                    | _ -> None
        | _ -> None

    let validateHeight (opt : string option) =
        match opt with
        | Some x -> let unit = x.Substring(x.Length - 2, 2)
                    let height = x.Substring(0, x.Length - 2) |> int
                    match (height, unit) with
                    | (h, "in") when 59 <= h && h <= 76 -> opt
                    | (h, "cm") when 150 <= h && h <= 193 -> opt
                    | _ -> None
        | _ -> None


    let parsePassport (data : string) =
        let map = data.Replace(" ", System.Environment.NewLine).Split(System.Environment.NewLine)
                |> Array.map (fun kv -> match kv.Split(":") with
                                        | [|key;value|] -> Some (key, value)
                                        | _ -> None)
                |> Array.choose id
                |> Map.ofArray
        {
            BirthYear = map.TryFind("byr") |> validateIntBetween 1920 2002
            IssueYear = map.TryFind("iyr") |> validateIntBetween 2010 2020
            ExpirationYear = map.TryFind("eyr") |> validateIntBetween 2020 2030
            Height = map.TryFind("hgt") |> validateRegex @"^\d+(in|cm)$" |> validateHeight
            HairColor = map.TryFind("hcl") |> validateRegex @"^#[0-9,a-z]{6}$"
            EyeColor = map.TryFind("ecl") |> validateRegex @"^(amb|blu|brn|gry|grn|hzl|oth)$"
            PassportId = map.TryFind("pid") |> validateRegex @"^\d{9}$"
            CountryId = map.TryFind("cid")
        }

    let isValid passport =
        match passport with
        | { BirthYear = None } -> false
        | { IssueYear = None } -> false
        | { ExpirationYear = None } -> false
        | { Height = None } -> false
        | { HairColor = None } -> false
        | { EyeColor = None } -> false
        | { PassportId = None } -> false
        | _ -> true // We allow empty CountryId

    data.Split((System.Environment.NewLine + System.Environment.NewLine))
    |> Array.map parsePassport
    |> Array.filter isValid