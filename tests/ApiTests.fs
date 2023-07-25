module ApiTests
open Fuchu

open DiscordBotExtensions.Api

[<Tests>]
let snowflakeTests =
    testList "snowflakeTests" [
        testCase "serialize and deserialize" <| fun () ->
            let converter = SnowflakeConverter()

            let ser x =
                Newtonsoft.Json.JsonConvert.SerializeObject(x, converter)

            let des (str: string) =
                Newtonsoft.Json.JsonConvert.DeserializeObject(str, converter)

            let sample = Snowflake.Create 12345UL

            let exp = "\"12345\""
            let act = ser sample

            Assert.Equal("", exp, act)

            let act: Snowflake = des exp

            Assert.Equal("", sample, act)
    ]
