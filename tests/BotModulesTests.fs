module BotModulesTests
open Fuchu

open DiscordBotExtensions

type Command =
    | Hello of string
    | Ping
    | Answer of int
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Command =
    module Parser =
        open FParsec

        let parse =
            PrefixCommandParser.initCommandParser [
                pstringCI "hello" >>. many1Satisfy (fun _ -> true) |>> Hello
                pstringCI "ping" >>% Ping
                pstringCI "answer" >>. spaces >>. pint32 |>> Answer
            ]

[<Tests>]
let commandParserTests =
    let prefix = "."
    let botId = 42UL

    let parse = PrefixCommandParser.start prefix botId Command.Parser.parse

    let botMention = sprintf "<@%d>" botId

    let createTest exp input =
        testCase input (fun _ ->
            Assert.Equal("", exp, parse input)
        )

    testList "commandParserTests" [
        createTest
            (Ok PrefixCommandParser.Pass)
            "some text"

        createTest
            (Ok PrefixCommandParser.Pass)
            "ping"

        createTest
            (Ok (PrefixCommandParser.Command Ping))
            ".ping"

        createTest
            (Ok PrefixCommandParser.Empty)
            "<@42>"

        createTest
            (Ok PrefixCommandParser.Unknown)
            "<@42> unknown"

        createTest
            (Ok PrefixCommandParser.Unknown)
            "<@42> .unknown"

        createTest
            (Ok (PrefixCommandParser.Command Ping))
            "<@42> ping"

        createTest
            (Ok (PrefixCommandParser.Command Ping))
            "<@42> .ping"

        let errMsg =
            [
                "Error in Ln: 1 Col: 8"
                ".answer"
                "       ^"
                "Note: The error occurred at the end of the input stream."
                "Expecting: integer number (32-bit, signed)"
                ""
            ] |> String.concat "\r\n"

        createTest
            (Error errMsg)
            ".answer"

        createTest
            (Ok (PrefixCommandParser.Command (Answer 42)))
            ".answer 42"
    ]
