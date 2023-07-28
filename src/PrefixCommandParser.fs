namespace DiscordBotExtensions

[<RequireQualifiedAccess>]
type PrefixCommandParser<'Command> =
    | Empty
    | Unknown
    | Pass

    | Command of 'Command

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PrefixCommandParser =
    open FParsec
    open FsharpMyExtension

    type 'a Parser = Parser<'a, unit>

    open DiscordMessage.Parser

    let initCommandParser (commands: Parser<_> seq): _ Parser =
        choice commands

    let start prefix botId (pcommand: _ Parser) =
        let prefix = pstring prefix
        let pcommand = pcommand |>> PrefixCommandParser.Command
        let p =
            (attempt (puserMentionTarget botId) >>. spaces
             >>. (
                (prefix >>. (pcommand <|>% PrefixCommandParser.Unknown))
                <|> pcommand
                <|> (eof >>% PrefixCommandParser.Empty)
                <|>% PrefixCommandParser.Unknown)
            )
            <|> (prefix >>. (pcommand <|>% PrefixCommandParser.Unknown))
            <|>% PrefixCommandParser.Pass

        FParsecExt.runResult p
