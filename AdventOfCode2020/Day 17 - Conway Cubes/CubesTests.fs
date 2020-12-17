module CubesTests

open FsUnit.Xunit
open Xunit
open Cubes
open Xunit.Abstractions

type CubesTests(output: ITestOutputHelper) =

    [<Fact>]
    member __.``Example Data`` () =
        let notes = [|
            ".#."
            "..#"
            "###"
        |]

        let active = scan notes 6

        active |> should equal 112

    [<Fact>]
    member __.``Puzzle Input`` () =
        let notes = [|
            "##..####"
            ".###...."
            "#.###.##"
            "#....#.."
            "...#..#."
            "#.#...##"
            "..#.#.#."
            ".##...#."
        |]

        let active = scan notes 6

        output.WriteLine("Active {0}", active)

    [<Fact>]
    member __.``Example Data - Part II`` () =
        let notes = [|
            ".#."
            "..#"
            "###"
        |]

        let active = scan2 notes 6

        active |> should equal 848

    [<Fact>]
    member __.``Puzzle Input - Part II`` () =
        let notes = [|
            "##..####"
            ".###...."
            "#.###.##"
            "#....#.."
            "...#..#."
            "#.#...##"
            "..#.#.#."
            ".##...#."
        |]

        let active = scan2 notes 6

        output.WriteLine("Active {0}", active)