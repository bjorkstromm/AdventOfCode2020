module AdaptersTests

open FsUnit.Xunit
open Xunit
open Adapters
open Xunit.Abstractions

type AdaptersTests(output: ITestOutputHelper) =

    [<Fact>]
    member __.``Example Data`` () =
        let input = [|
            28
            33
            18
            42
            31
            14
            46
            20
            48
            47
            24
            23
            49
            45
            19
            38
            39
            11
            1
            32
            25
            35
            8
            17
            7
            9
            4
            2
            34
            10
            3
        |]

        let r = joltage input
        r |> should equal (22, 1, 10)

    [<Fact>]
    member __.``Puzzle input`` () =
        let input = [|
            26
            97
            31
            7
            2
            10
            46
            38
            112
            54
            30
            93
            18
            111
            29
            75
            139
            23
            132
            85
            78
            99
            8
            113
            87
            57
            133
            41
            104
            98
            58
            90
            13
            91
            20
            68
            103
            127
            105
            114
            138
            126
            67
            32
            145
            115
            16
            141
            1
            73
            45
            119
            51
            40
            35
            150
            118
            53
            80
            79
            65
            135
            74
            47
            128
            64
            17
            4
            84
            83
            147
            142
            146
            9
            125
            94
            140
            131
            134
            92
            66
            122
            19
            86
            50
            52
            108
            100
            71
            61
            44
            39
            3
            72
        |]

        let (x, _, y) = joltage input
        output.WriteLine("{0} * {1} = {2}", x, y, (x*y))

    [<Fact>]
    member __.``Example Data - Part II`` () =
        let input = [|
            16
            10
            15
            5
            1
            11
            7
            19
            6
            12
            4
        |]

        combinations input
