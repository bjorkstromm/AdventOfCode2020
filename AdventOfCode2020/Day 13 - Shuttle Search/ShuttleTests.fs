module ShuttleTests

open FsUnit.Xunit
open Xunit
open Shuttle
open Xunit.Abstractions

type ShuttleTests(output: ITestOutputHelper) =

    [<Fact>]
    member __.``Example Data`` () =
        let input = [|
            "939"
            "7,13,x,x,59,x,31,19"
        |]

        let (id, minute) = search input
        minute |> should equal 5
        id |> should equal 59
        minute * id |> should equal 295


    [<Fact>]
    member __.``Puzzle Input`` () =
        let input = [|
            "1008713"
            "13,x,x,41,x,x,x,x,x,x,x,x,x,467,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,353,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23"
        |]

        let (id, minute) = search input
        output.WriteLine("{0} x {1} = {2}", minute, id, minute * id)


    [<Fact>]
    member __.``Example Data`` () =
        let input = "7,13,x,x,59,x,31,19"

        let earliest = search2 input
        earliest |> should equal 1068788UL