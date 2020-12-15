module MemoryTests

open FsUnit.Xunit
open Xunit
open Memory
open Xunit.Abstractions

type MemoryTests(output: ITestOutputHelper) =

    [<Fact>]
    member __.``Example Data`` () =
        let input = [| 0 ; 3 ; 6 |]

        let r = play input 2020

        r |> Array.length |> should equal 2020
        r |> Array.last |> should equal 436

    [<Fact>]
    member __.``Puzzle Input`` () =
        let input = [| 1 ; 20 ; 11 ; 6 ; 12 ; 0 |]

        let r = play input 2020

        r |> Array.length |> should equal 2020

        output.WriteLine("{0}", r |> Array.last)


    [<Fact>]
    member __.``Example Data - Part II`` () =
        let input = [| 0 ; 3 ; 6 |]

        let r = play input 30000000

        r |> Array.length |> should equal 30000000
        r |> Array.last |> should equal 175594

    [<Fact>]
    member __.``Puzzle Input - Part II`` () =
        let input = [| 1 ; 20 ; 11 ; 6 ; 12 ; 0 |]

        let r = play input 30000000

        r |> Array.length |> should equal 30000000

        output.WriteLine("{0}", r |> Array.last)