module SimpleModule.Main
open FsharpMyExtension
open DSharpPlus

open Shared
open Types

type Action =
    | Hello of name: string
    | Reload
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

        let preload: _ Parser =
            pstring "reload"

        let start f: _ Parser =
            choice [
                phello |>> Hello
                preload >>% Reload
            ]
            >>= fun msg ->
                preturn (fun x -> f x msg)

    let help prefix =
        sprintf "`%shello <text>`" prefix

type Msg =
    | Request of (DiscordClient * EventArgs.MessageCreateEventArgs) * Action

let request = function
    | Request((client, e), act) ->
        awaiti <| e.Channel.TriggerTypingAsync()

        match act with
        | Hello name ->
            let msg =
                sprintf "Hello, %s" name

            awaiti <| e.Channel.SendMessageAsync(msg)

        | Reload ->
            awaiti <| e.Channel.SendMessageAsync("reloading...")

            awaiti <| client.ReconnectAsync(true)

let create () =
    { BotModule.empty with
        MessageCreateEventHandleExclude =
            let exec: _ Action.Parser.Parser =
                Action.Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    request (Request((client, e), msg))
                )
            Some exec
        Scheduler =
            let f (client: DiscordClient) =
                let scheduler = Scheduler.Scheduler Scheduler.State.Empty
                scheduler.AddJob {
                    Time = System.DateTime.Now.AddMinutes 1
                    Type = ()
                }

                printfn "starting scheduler..."

                Scheduler.startAsync scheduler 100 (fun task ->
                    printfn "Ding!"

                    scheduler.AddJob {
                        Time = System.DateTime.Now.AddMinutes 1
                        Type = ()
                    }
                )

            Some f
    }
