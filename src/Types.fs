module DiscordBotExtensions.Types
open FsharpMyExtension
open FsharpMyExtension.Containers

type GuildId = uint64
type UserId = uint64
type ChannelId = uint64
type MessageId = uint64
type EmojiId = uint64
type RoleId = uint64
type WebhookId = uint64

type ChannelPath =
    {
        GuildId: GuildId
        ChannelId: ChannelId
    }

type EnabledOptionValue<'Value> =
    {
        IsEnabled: bool
        Value: 'Value option
    }
    static member Empty: EnabledOptionValue<'Value> =
        {
            IsEnabled = false
            Value = None
        }
    static member Init (v: 'Value) =
        {
            IsEnabled = true
            Value = Some v
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module EnabledOptionValue =
    let fold folder (st: 'State) (v: EnabledOptionValue<'Value>) =
        if v.IsEnabled then
            match v.Value with
            | Some x -> folder x
            | None -> st
        else
            st

    let map mapping (v: EnabledOptionValue<'a>): EnabledOptionValue<'b> =
        match v.Value with
        | Some oldValue ->
            {
                IsEnabled = v.IsEnabled
                Value = Some (mapping oldValue)
            }
        | None ->
            {
                IsEnabled = v.IsEnabled
                Value = None
            }

    let toOption (v: EnabledOptionValue<'a>): 'a option =
        if v.IsEnabled then
            v.Value
        else
            None

type ResultView =
    {
        View: DSharpPlus.Entities.DiscordMessageBuilder option
        ResponseToUser: DSharpPlus.Entities.DiscordMessageBuilder option
    }
