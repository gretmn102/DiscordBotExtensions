module Program
open Microsoft.Extensions.Logging
open System.Threading.Tasks
open DSharpPlus

open Types

module ExampleModule =
    open Shared

    type Action =
        | Hello of name: string
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Action =
        module Parser =
            open FParsec

            open DiscordMessage.Parser

            type 'a Parser = Parser<'a, unit>

            let phello: _ Parser =
                pstring "hello"
                >>. manySatisfy (fun _ -> true)

            let start f: _ Parser =
                choice [
                    phello |>> Hello
                ]
                >>= fun msg ->
                    preturn (fun x -> f x msg)

        let help prefix =
            sprintf "`%shello <text>`" prefix

    type Msg =
        | Request of (DiscordClient * EventArgs.MessageCreateEventArgs) * Action

    let request = function
        | Request((client, e), act) ->
            match act with
            | Hello name ->
                awaiti <| e.Channel.TriggerTypingAsync()

                let msg =
                    sprintf "Hello, %s" name

                awaiti <| e.Channel.SendMessageAsync(msg)

    let create () =
        { BotModule.empty with
            MessageCreateEventHandleExclude =
                let exec: _ Action.Parser.Parser =
                    Action.Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                        request (Request((client, e), msg))
                    )
                Some exec
        }

let botEventId = new EventId(42, "Bot-Event")

let initBotModules () =
    [|
        ExampleModule.create ()
    |]

[<EntryPoint>]
let main argv =
    let getBotToken next =
        let tokenEnvVar = "BotToken"
        match tryGetEnvironmentVariable tokenEnvVar with
        | None ->
            printfn "Environment variable `%s` is not set!" tokenEnvVar
            1
        | Some token ->
            next token

    getBotToken <| fun token ->
    let config = DSharpPlus.DiscordConfiguration()

    config.set_Token token
    config.set_TokenType DSharpPlus.TokenType.Bot
    config.set_AutoReconnect true
    config.set_Intents (
        DSharpPlus.DiscordIntents.AllUnprivileged
        ||| DSharpPlus.DiscordIntents.GuildMembers
        ||| DSharpPlus.DiscordIntents.GuildPresences
        ||| DSharpPlus.DiscordIntents.MessageContents
    )

    let client = new DSharpPlus.DiscordClient(config)

    let botModules = initBotModules ()

    let prefix = "."

    botModules
    |> Shared.BotModule.bindToClientsEvents
        prefix
        (fun client e ->
            let b = Entities.DiscordMessageBuilder()
            let embed = Entities.DiscordEmbedBuilder()
            embed.Description <- ExampleModule.Action.help prefix
            b.Embed <- embed
            awaiti <| e.Channel.SendMessageAsync(b)
        )
        (fun client e ->
            let b = Entities.DiscordMessageBuilder()
            let embed = Entities.DiscordEmbedBuilder()
            embed.Description <-
                sprintf "Unknown command:\n%s" (ExampleModule.Action.help prefix)
            b.Embed <- embed
            awaiti <| e.Channel.SendMessageAsync(b)
        )
        (fun _ _ -> ())
        client

    client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client readyEventArgs ->
        client.Logger.LogInformation(botEventId, "Client is ready to process events.")

        Task.CompletedTask
    ))

    client.add_ClientErrored(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
        client.Logger.LogError(botEventId, e.Exception, "Exception occured", [||])

        Task.CompletedTask
    ))

    client.add_GuildDownloadCompleted(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
        let status =
            "Test"

        let activity = DSharpPlus.Entities.DiscordActivity(status)
        awaiti <| client.UpdateStatusAsync(activity)

        Task.CompletedTask
    ))

    awaiti <| client.ConnectAsync()

    awaiti <| Task.Delay -1

    0
