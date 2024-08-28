module MvcExample.Main
open FsharpMyExtension.Control
open FsharpMyExtension.Control.Task
open DSharpPlus

open DiscordBotExtensions
open DiscordBotExtensions.Extensions

open View

type Action =
    | Hello of name: string
    | Reconnect
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Action =
    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'a Parser = Parser<'a, unit>

        let phello: _ Parser =
            pstring "helloMvc"
            >>. manySatisfy (fun _ -> true)

        let preload: _ Parser =
            pstring "reconnectMvc"

        let start f: _ Parser =
            choice [
                phello |>> Hello
                preload >>% Reconnect
            ]
            >>= fun msg ->
                preturn (fun x -> f x msg)

    let help prefix =
        sprintf "`%shello <text>`" prefix

type Msg =
    | Request of (DiscordClient * EventArgs.MessageCreateEventArgs) * Action
    | RequestSlashCommand of EventArgs.InteractionCreateEventArgs * Action

type State =
    {
        MvcState: Mvc.Controller.State<Model.ViewCmd, Model.MyCmd>
    }

let interpView (view: Model.ViewCmd) =
    match view with
    | Model.ViewCmd.Reconnect ->
        ReloadView.create ()
    | Model.ViewCmd.Hello name ->
        HelloView.create name

let interp api (client: DiscordClient) (req: Model.MyCmd) (state: State) =
    let rec interp cmd state =
        match cmd with
        | Model.MyCmd.MvcCmd cmd ->
            let cmd, state' =
                Mvc.Controller.interp api cmd state.MvcState

            let state =
                { state with
                    MvcState = state'
                }

            interp cmd state

        | Model.MyCmd.Reconnect((), next) ->
            awaiti <| client.ReconnectAsync(true)

            let cmd = next ()

            interp cmd state

        | Model.MyCmd.End -> state

    interp req state

let reduce (client: DiscordClient) (restClient: DiscordRestClient) (cmd: Msg) (state: State) =
    match cmd with
    | Request((client, e), act) ->
        awaiti <| e.Channel.TriggerTypingAsync()

        let interp =
            let api =
                Mvc.Controller.createMessageApi
                    interpView
                    (fun state ->
                        let req = Model.MyCmd.End
                        req, state
                    )
                    restClient
                    e

            interp api client

        match act with
        | Hello name ->
            interp (Model.hello name) state

        | Reconnect ->
            interp (Model.reload ()) state

    | RequestSlashCommand(e, act) ->
        let interp =
            let api =
                Mvc.Controller.createSlashCommandApi
                    interpView
                    (fun state ->
                        let req = Model.MyCmd.End
                        req, state
                    )
                    restClient
                    e

            interp api client

        match act with
        | Hello name ->
            interp (Model.hello name) state

        | Reconnect ->
            interp (Model.reload ()) state


let create (client: DiscordClient) (restClient: DiscordRestClient) =
    let m =
        let init: State = {
            MvcState = Mvc.Controller.State.empty
        }

        let x = init.MvcState.Deferreds.Jobs[System.DateTime.Now]
        interp api client x.Type init

        MailboxProcessor.Start (fun mail ->
            let rec loop (state: State) =
                async {
                    let! msg = mail.Receive()
                    let state =
                        try
                            reduce client restClient msg state
                        with e ->
                            printfn "%A" e
                            state

                    return! loop state
                }
            loop init
        )

    let commands =
        let hello =
            let slashCommandName = "hello"
            let targetOptionName = "name"
            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
                Command =
                    let targetOption =
                        Entities.DiscordApplicationCommandOption(
                            targetOptionName,
                            "name",
                            ApplicationCommandOptionType.String,
                            required = true
                        )

                    new Entities.DiscordApplicationCommand(
                        slashCommandName,
                        "prompt `Hello, %name%`",
                        ``type`` = ApplicationCommandType.SlashCommand,
                        options = [
                            targetOption
                        ]
                    )
                Handler = fun e ->
                    let getName next =
                        let res =
                            e.Interaction.Data.Options
                            |> Seq.tryFind (fun x -> x.Name = targetOptionName)

                        match res with
                        | Some opt ->
                            let targetId = opt.Value :?> string
                            next targetId
                        | None -> ()

                    getName <| fun name ->
                    m.Post(RequestSlashCommand(e, Hello name))
            |}

        let reconnect =
            let slashCommandName = "reconnect"
            InteractionCommand.SlashCommand {|
                CommandName = slashCommandName
                Command =
                    new Entities.DiscordApplicationCommand(
                        slashCommandName,
                        "reconnect current bot session",
                        ``type`` = ApplicationCommandType.SlashCommand
                    )
                Handler = fun e ->
                    m.Post(RequestSlashCommand(e, Reconnect))
            |}

        [|
            hello
            reconnect
        |]

    { BotModule.empty with
        InteractionCommands =
            Some commands

        MessageCreateEventHandleExclude =
            let exec: _ Action.Parser.Parser =
                Action.Parser.start (fun (client: DiscordClient, e: EventArgs.MessageCreateEventArgs) msg ->
                    m.Post(Request((client, e), msg))
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
