module StandardDiscordEmojisTests
open Fuchu

open DiscordBotExtensions.StandardDiscordEmojisContainer

[<Tests>]
let StandardDiscordEmojisTests =
    testList "StandardDiscordEmojisContainerTests" [
        testCase "base" <| fun () ->
            // just test that resources are loaded correctly
            let exp =
                (23, 2)
            let act =
                emojiSheetMap.["🎃"]

            Assert.Equal("", exp, act)
    ]
