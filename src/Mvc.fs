module DiscordBotExtensions.Mvc
open DSharpPlus
open Types

module Model =
    type Req<'Arg, 'Res, 'Next> = 'Arg * ('Res -> 'Next)

    type InteractionData =
        {
            Id: uint64
            Token: string
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module InteractionData =
        let create applicationId token : InteractionData =
            {
                Id = applicationId
                Token = token
            }

    [<RequireQualifiedAccess>]
    type EphemeralResponsesReq<'Next> =
        | Add of Req<MessageId * InteractionData, unit, 'Next>
        | Get of Req<MessageId, InteractionData option, 'Next>
        | Remove of Req<MessageId, unit, 'Next>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module EphemeralResponsesReq =
        let add arg next =
            EphemeralResponsesReq.Add(arg, next)

        let get arg next =
            EphemeralResponsesReq.Get(arg, next)

        let remove arg next =
            EphemeralResponsesReq.Remove(arg, next)

    module EphemeralResponses =
        type LocalMessagePath =
            {
                ChannelId: ChannelId
                MessageId: MessageId
            }
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        [<RequireQualifiedAccess>]
        module LocalMessagePath =
            let create channelId messageId =
                {
                    ChannelId = channelId
                    MessageId = messageId
                }

        type State = Map<LocalMessagePath, InteractionData>

        let empty: State = Map.empty

        let interp channelId (req: EphemeralResponsesReq<_>) (state: State) =
            match req with
            | EphemeralResponsesReq.Get(messageId, next) ->
                let id = LocalMessagePath.create channelId messageId
                let req =
                    Map.tryFind id state
                    |> next
                req, state

            | EphemeralResponsesReq.Add((messageId, data), next) ->
                let id = LocalMessagePath.create channelId messageId
                let state =
                    Map.add id data state
                next (), state

            | EphemeralResponsesReq.Remove(messageId, next) ->
                let id = LocalMessagePath.create channelId messageId
                let state =
                    Map.remove id state
                next (), state

    [<RequireQualifiedAccess>]
    type Cmd<'View, 'Next> =
        | EphemeralResponsesReq of EphemeralResponsesReq<'Next>

        | GetCurrentMessageId of Req<unit, MessageId option, 'Next>
        | UserIsBot of Req<UserId, bool, 'Next>
        | GetReferenceMessageId of Req<unit, MessageId option, 'Next>
        | CreateView of Req<{| Reference: MessageId option; View: 'View |}, MessageId option, 'Next>
        | UpdateView of Req<{| MessageId: MessageId; View: 'View |}, unit, 'Next>
        | RemoveCurrentView of Req<InteractionData option, unit, 'Next>
        | GetInteractionData of Req<unit, InteractionData option, 'Next>

        | ResponseCreateView of Req<{| IsEphemeral: bool; View: 'View |}, MessageId option, 'Next>
        | ResponseUpdateCurrentView of Req<'View, unit, 'Next>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Cmd =
        let ephemeralResponsesReq fn arg next =
            Cmd.EphemeralResponsesReq (fn arg (fun res ->
                next res
            ))

        let getCurrentMessageId () next =
            Cmd.GetCurrentMessageId((), next)

        let userIsBot userId next =
            Cmd.UserIsBot(userId, next)

        let getReferenceMessageId arg next =
            Cmd.GetReferenceMessageId(arg, next)

        let getInteractionData arg next =
            Cmd.GetInteractionData(arg, next)

        let responseCreateView isEphemeral view next =
            let opts =
                {| IsEphemeral = isEphemeral; View = view |}
            Cmd.ResponseCreateView(opts, next)

        let createView reference view next =
            let opts =
                {| View = view; Reference = reference |}
            Cmd.CreateView(opts, next)

        let updateView messageId view next =
            let opts =
                {| MessageId = messageId; View = view |}
            Cmd.UpdateView(opts, next)

        let removeCurrentView arg next =
            Cmd.RemoveCurrentView(arg, next)

        let responseUpdateCurrentView view next =
            Cmd.ResponseUpdateCurrentView(view, next)

module Controller =
    type State =
        {
            EphemeralResponses: Model.EphemeralResponses.State
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module State =
        let empty: State =
            {
                EphemeralResponses = Model.EphemeralResponses.empty
            }

    type Api<'View, 'Next> =
        abstract GetCurrentChannelId : unit -> ChannelId
        abstract GetCurrentMessageId : unit -> MessageId option
        abstract InterpView : 'View -> Entities.DiscordMessageBuilder
        abstract ReturnError : State -> 'Next * State
        abstract ResponseCreate : bool -> Entities.DiscordMessageBuilder -> option<MessageId>
        abstract ResponseUpdate : Entities.DiscordMessageBuilder -> unit
        abstract UpdateMessage : MessageId -> Entities.DiscordMessageBuilder -> unit
        abstract CreateMessage : option<MessageId> -> Entities.DiscordMessageBuilder -> option<MessageId>
        abstract RemoveCurrent : option<Model.InteractionData> -> unit
        abstract GetReference : unit -> option<MessageId>
        abstract GetMemberAsync : UserId -> System.Threading.Tasks.Task<Entities.DiscordMember>
        abstract GetInteractionData : unit -> option<Model.InteractionData>

    let interp (api: Api<'View, 'Next>) (cmd: Model.Cmd<'View,'Next>) state =
        let interp (cmd: Model.Cmd<'View,'Next>) state =
            match cmd with
            | Model.Cmd.EphemeralResponsesReq req ->
                let req, ephemeralResponses =
                    let channelId = api.GetCurrentChannelId()
                    Model.EphemeralResponses.interp channelId req state.EphemeralResponses

                let state =
                    { state with
                        EphemeralResponses = ephemeralResponses
                    }

                req, state

            | Model.Cmd.ResponseCreateView(view, next) ->
                let messageId =
                    api.InterpView view.View
                    |> api.ResponseCreate view.IsEphemeral

                let req =
                    next messageId

                req, state

            | Model.Cmd.CreateView(opts, next) ->
                let messageId =
                    api.InterpView opts.View
                    |> api.CreateMessage opts.Reference

                let req =
                    next messageId

                req, state

            | Model.Cmd.ResponseUpdateCurrentView(view, next) ->
                let req =
                    api.InterpView view
                    |> api.ResponseUpdate
                    |> next

                req, state

            | Model.Cmd.UpdateView(opts, next) ->
                let req =
                    api.InterpView opts.View
                    |> api.UpdateMessage opts.MessageId
                    |> next

                req, state

            | Model.Cmd.RemoveCurrentView(interactionDataOpt, next) ->
                let req =
                    api.RemoveCurrent interactionDataOpt
                    |> next

                req, state

            | Model.Cmd.GetCurrentMessageId((), next) ->
                let req = next (api.GetCurrentMessageId ())

                req, state

            | Model.Cmd.UserIsBot(userId, userIdBot) ->
                let user =
                    try
                        let guildMember: Entities.DiscordMember = await <| api.GetMemberAsync userId
                        Ok guildMember
                    with e ->
                        Error e.Message

                match user with
                | Ok user ->
                    let req = userIdBot user.IsBot

                    req, state
                | Error(errorValue) ->
                    let b = Entities.DiscordMessageBuilder()
                    b.Content <- sprintf "```\n%s\n```" errorValue
                    let messageId = api.ResponseCreate true b

                    api.ReturnError state

            | Model.Cmd.GetReferenceMessageId((), next) ->
                let req =
                    next (api.GetReference ())

                req, state

            | Model.Cmd.GetInteractionData((), next) ->
                let req =
                    next (api.GetInteractionData ())

                req, state

        interp cmd state

    let createSlashCommandApi interpView returnError (restClient: DiscordRestClient) (e: EventArgs.InteractionCreateEventArgs) =
        let responseCreate isEphemeral (b: Entities.DiscordMessageBuilder) =
            let b = Entities.DiscordInteractionResponseBuilder(b)
            b.IsEphemeral <- isEphemeral
            let typ =
                InteractionResponseType.ChannelMessageWithSource
            awaiti <| e.Interaction.CreateResponseAsync (typ, b)
            None

        { new Api<'View, 'Next> with
            member _.GetCurrentMessageId(): ChannelId option =
                None

            member _.CreateMessage(referenceMessageIdOpt: MessageId option) (b: Entities.DiscordMessageBuilder): MessageId option =
                referenceMessageIdOpt
                |> Option.iter (fun messageId ->
                    b.WithReply(messageId, true)
                    |> ignore
                )

                let message = await <| e.Interaction.Channel.SendMessageAsync(b)
                Some message.Id

            member _.GetCurrentChannelId(): ChannelId =
                e.Interaction.ChannelId

            member _.GetInteractionData(): Model.InteractionData option =
                Model.InteractionData.create
                    e.Interaction.ApplicationId
                    e.Interaction.Token
                |> Some

            member _.GetMemberAsync(userId: UserId): System.Threading.Tasks.Task<Entities.DiscordMember> =
                e.Interaction.Guild.GetMemberAsync userId

            member _.GetReference(): MessageId option =
                None

            member _.InterpView(arg1: 'View): Entities.DiscordMessageBuilder =
                interpView arg1

            member _.RemoveCurrent(arg1: Model.InteractionData option): unit =
                ()

            member _.ResponseCreate(isEphemeral: bool) (b: Entities.DiscordMessageBuilder): MessageId option =
                responseCreate isEphemeral b

            member _.ResponseUpdate(b: Entities.DiscordMessageBuilder): unit =
                responseCreate false b |> ignore

            member _.ReturnError(state: State): 'Next * State =
                returnError state

            member _.UpdateMessage(messageId: MessageId) (b: Entities.DiscordMessageBuilder): unit =
                awaiti <| restClient.EditMessageAsync(e.Interaction.ChannelId, messageId, b)
        }

    let createComponentInteractionApi interpView returnError (restClient: DiscordRestClient) (e: EventArgs.ComponentInteractionCreateEventArgs) =
        { new Api<'View, 'Next> with
            member _.GetCurrentMessageId(): ChannelId option =
                Some e.Message.Id

            member _.CreateMessage(referenceMessageIdOpt: MessageId option) (b: Entities.DiscordMessageBuilder): MessageId option =
                referenceMessageIdOpt
                |> Option.iter (fun messageId ->
                    b.WithReply(messageId, true)
                    |> ignore
                )

                let message = await <| e.Interaction.Channel.SendMessageAsync(b)
                Some message.Id

            member _.GetCurrentChannelId(): ChannelId =
                e.Interaction.ChannelId

            member _.GetInteractionData(): Model.InteractionData option =
                Model.InteractionData.create
                    e.Interaction.ApplicationId
                    e.Interaction.Token
                |> Some

            member _.GetMemberAsync(userId: UserId): System.Threading.Tasks.Task<Entities.DiscordMember> =
                e.Interaction.Guild.GetMemberAsync userId

            member _.GetReference(): MessageId option =
                e.Message.Reference
                |> Option.ofObj
                |> Option.map (fun r -> r.Message.Id)

            member _.InterpView(arg1: 'View): Entities.DiscordMessageBuilder =
                interpView arg1

            member _.RemoveCurrent(interactionDataOpt: Model.InteractionData option): unit =
                match interactionDataOpt with
                | Some interactionData ->
                    try
                        awaiti <| restClient.DeleteWebhookMessageAsync(interactionData.Id, interactionData.Token, e.Message.Id)
                    with e ->
                        ()
                | None ->
                    try
                        awaiti <| e.Message.DeleteAsync()
                    with e ->
                        ()

            member _.ResponseCreate(isEphemeral: bool) (b: Entities.DiscordMessageBuilder): MessageId option =
                let emptyResponseWithEphemeral = Entities.DiscordInteractionResponseBuilder()
                emptyResponseWithEphemeral.IsEphemeral <- isEphemeral
                let typ =
                    InteractionResponseType.DeferredChannelMessageWithSource
                awaiti <| e.Interaction.CreateResponseAsync(typ, emptyResponseWithEphemeral)

                let b = Entities.DiscordFollowupMessageBuilder(b)
                let message =
                    await <| e.Interaction.CreateFollowupMessageAsync b

                Some message.Id

            member _.ResponseUpdate(b: Entities.DiscordMessageBuilder): unit =
                let b = Entities.DiscordInteractionResponseBuilder(b)
                let typ =
                    InteractionResponseType.UpdateMessage
                awaiti <| e.Interaction.CreateResponseAsync (typ, b)

            member _.ReturnError(state: State): 'Next * State =
                returnError state

            member _.UpdateMessage(messageId: MessageId) (b: Entities.DiscordMessageBuilder): unit =
                awaiti <| restClient.EditMessageAsync(e.Interaction.ChannelId, messageId, b)
        }

    let createMessageApi interpView returnError (restClient: DiscordRestClient) (e: EventArgs.MessageCreateEventArgs) =
        { new Api<'View, 'Next> with
            member _.GetCurrentMessageId(): ChannelId option =
                Some e.Message.Id

            member _.CreateMessage(referenceMessageIdOpt: MessageId option) (b: Entities.DiscordMessageBuilder): MessageId option =
                referenceMessageIdOpt
                |> Option.iter (fun messageId ->
                    b.WithReply(messageId, true)
                    |> ignore
                )

                let message = await <| e.Channel.SendMessageAsync(b)
                Some message.Id

            member _.GetCurrentChannelId(): ChannelId =
                e.Channel.Id

            member _.GetInteractionData(): Model.InteractionData option =
                None

            member _.GetMemberAsync(userId: UserId): System.Threading.Tasks.Task<Entities.DiscordMember> =
                e.Guild.GetMemberAsync userId

            member _.GetReference(): MessageId option =
                e.Message.Reference
                |> Option.ofObj
                |> Option.map (fun r -> r.Message.Id)

            member _.InterpView(arg1: 'View): Entities.DiscordMessageBuilder =
                interpView arg1

            member _.RemoveCurrent(interactionDataOpt: Model.InteractionData option): unit =
                match interactionDataOpt with
                | Some interactionData ->
                    try
                        awaiti <| restClient.DeleteWebhookMessageAsync(interactionData.Id, interactionData.Token, e.Message.Id)
                    with e ->
                        ()
                | None ->
                    try
                        awaiti <| e.Message.DeleteAsync()
                    with e ->
                        ()

            member _.ResponseCreate(isEphemeral: bool) (b: Entities.DiscordMessageBuilder): MessageId option =
                // TODO: implemented `isEmphemeral`

                b.WithReply(e.Message.Id, true)
                |> ignore

                let message = await <| e.Channel.SendMessageAsync(b)
                Some message.Id

            member _.ResponseUpdate(b: Entities.DiscordMessageBuilder): unit =
                awaiti <| e.Message.ModifyAsync(b)

            member _.ReturnError(state: State): 'Next * State =
                returnError state

            member _.UpdateMessage(messageId: MessageId) (b: Entities.DiscordMessageBuilder): unit =
                awaiti <| restClient.EditMessageAsync(e.Channel.Id, messageId, b)
        }
