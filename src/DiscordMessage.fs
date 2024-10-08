namespace DiscordBotExtensions.DiscordMessage
open DiscordBotExtensions.Types

type CustomEmoji =
    {
        Id: EmojiId
        Animated: bool
        Name: string
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module CustomEmoji =
    open FsharpMyExtension.Serialization.Deserializers

    module Parser =
        open FParsec

        let parser<'u> : Parser<_, 'u> =
            pipe3
                (stringReturn "<:" false <|> stringReturn "<a:" true)
                (manySatisfy ((<>) ':') .>> skipChar ':')
                (puint64 .>> pchar '>')
                (fun animated name id ->
                    {
                        Id = id
                        Animated = animated
                        Name = name
                    }
                )

    let toString (customEmoji: CustomEmoji) =
        sprintf "<:%s:%d>" customEmoji.Name customEmoji.Id

    let parse =
        FParsec.runResult Parser.parser

type UnicodeOrCustomEmoji =
    | UnicodeEmoji of string
    | CustomEmoji of CustomEmoji
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UnicodeOrCustomEmoji =
    open FsharpMyExtension.Serialization.Deserializers

    module Parser =
        open FParsec

        let punicodeEmoji<'u> : Parser<_, 'u> =
            many1Satisfy ((<>) ' ') // TODO!

        let parser<'u> : Parser<_, 'u> =
            CustomEmoji.Parser.parser |>> CustomEmoji <|> (punicodeEmoji |>> UnicodeEmoji)

    let toString (unicodeOrCustomEmoji: UnicodeOrCustomEmoji) =
        match unicodeOrCustomEmoji with
        | CustomEmoji x ->
            sprintf "<:%s:%d>" x.Name x.Id
        | UnicodeEmoji emoji ->
            emoji

    let parse =
        FParsec.runResult Parser.parser

type MessagePath =
    {
        GuildId: GuildId
        ChannelId: ChannelId
        MessageId: MessageId
    }
    static member OfDiscordMessage (msg: DSharpPlus.Entities.DiscordMessage) =
        {
            GuildId = msg.Channel.Guild.Id
            ChannelId = msg.Channel.Id
            MessageId = msg.Id
        }
    member this.ToDiscordPath =
        sprintf "https://discord.com/channels/%d/%d/%d" this.GuildId this.ChannelId this.MessageId
    member this.ToChannelPath =
        {
            GuildId = this.GuildId
            ChannelId = this.ChannelId
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MessagePath =
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Parser =
        open FParsec

        let parser<'u> : Parser<_, 'u> =
            pipe3
                (skipString "https://discord.com/channels/" >>. puint64 .>> pchar '/')
                (puint64 .>> pchar '/')
                puint64
                (fun guildId channelId messageId ->
                    {
                        GuildId = guildId
                        ChannelId = channelId
                        MessageId = messageId
                    }
                )

module Parser =
    open FParsec

    let puserMention<'u> : Parser<UserId, 'u> =
        skipString "<@" >>. optional (skipChar '!') >>. puint64 .>> skipChar '>'

    let puserMentionTargetP (p: Parser<_, 'u>) : Parser<_, 'u> =
        skipString "<@" >>? optional (skipChar '!') >>? p .>> skipChar '>'

    let puserMentionTargetStr<'u> (userId: string) : Parser<_, 'u> =
        puserMentionTargetP (skipString userId)

    let puserMentionTarget<'u> (userId: UserId) : Parser<_, 'u> =
        puserMentionTargetStr (string userId)

    let pmentionRole<'u> : Parser<RoleId, 'u> =
        skipString "<@&" >>. puint64 .>> skipChar '>'

    let pmentionRoleTargetStr<'u> (roleId: string): Parser<_, 'u> =
        skipString "<@&" >>? skipString roleId .>> skipChar '>'

    let pmentionRoleTarget<'u> (roleId: RoleId): Parser<_, 'u> =
        pmentionRoleTargetStr (string roleId)

    let pchannelMention<'u> : Parser<ChannelId, 'u> =
        skipString "<#" >>. puint64 .>> skipChar '>'

    let pchannelMentionTargetStr<'u> (channelId: string): Parser<_, 'u> =
        skipString "<#" >>? skipString channelId .>> skipChar '>'

    let pchannelMentionTarget<'u> (channelId: ChannelId): Parser<_, 'u> =
        pchannelMentionTargetStr (string channelId)

    let pmessagePath<'u> : Parser<_, 'u> = MessagePath.Parser.parser<'u>

    let pemoji<'u> : Parser<_, 'u> =
        UnicodeOrCustomEmoji.Parser.parser<'u>

    let pcodeBlock<'u> : Parser<_, 'u> =
        between (skipString "```" .>> skipManySatisfy ((<>) '\n') .>> skipChar '\n')
            (skipString "```")
            (manyStrings (
                many1Satisfy ((<>) '`')
                <|> (notFollowedByString "```" >>. charReturn '`' "`"))
            )

    let pquote<'u> : Parser<_, 'u> =
        between
            (skipChar '"')
            (skipChar '"')
            (many1Strings (
                many1Satisfy (isNoneOf "\"\\")
                <|> (skipChar '\\' >>. ((pchar '"' |>> string) <|>% "\\"))
            ))

module Ext =
    open FsharpMyExtension.Control.Task

    let clearComponents (msg: DSharpPlus.Entities.DiscordMessage) =
        // does not clean components:
        // let content = DSharpPlus.Entities.Optional.FromValue ""
        // awaiti (msg.ModifyAsync content)

        let b = DSharpPlus.Entities.DiscordMessageBuilder()
        // necessary because throw `System.ArgumentException: You must specify content, an embed, a sticker, or at least one file.`
        b.AddEmbeds msg.Embeds |> ignore
        b.Content <- msg.Content
        awaiti (msg.ModifyAsync b)
