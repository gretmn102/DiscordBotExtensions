module DiscordBotExtensions.DiscordGuild
open Types

let getMember (user: DSharpPlus.Entities.DiscordUser) (guild: DSharpPlus.Entities.DiscordGuild) =
    match user with
    | :? DSharpPlus.Entities.DiscordMember as guildMember -> guildMember
    | user -> await (guild.GetMemberAsync user.Id)
