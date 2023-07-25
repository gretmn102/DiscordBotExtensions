module StandardDiscordEmojisTests
open Fuchu

open DiscordBotExtensions
open Types.StandartDiscordEmoji

[<Tests>]
let StandardDiscordEmojisTests =
    testList "StandardDiscordEmojisTests" [
        testCase "base" <| fun () ->
            // just test that resources are loaded correctly
            let exp =
                (23, 2)
            let act =
                emojiSheetMap.["🎃"]

            Assert.Equal("", exp, act)
    ]
