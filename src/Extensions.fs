module Extensions
open FsharpMyExtension
open FsharpMyExtension.Either
open DSharpPlus

module DiscordEmbed =
    let backgroundColorDarkTheme = Entities.DiscordColor("#2f3136")

module Interaction =
    open Newtonsoft.Json

    type RawComponentId = int32

    type FormId = string

    type ComponentState<'ComponentId, 'Data when 'ComponentId: enum<RawComponentId>> =
        {
            Id: FormId
            [<JsonProperty("CI")>]
            ComponentId: 'ComponentId
            [<JsonProperty("D")>]
            Data: 'Data
        }
        static member Serialize (x: ComponentState<'ComponentId, 'Data>) =
            Json.serNotIndent x

        static member Deserialize (s: string): Result<ComponentState<'ComponentId, 'Data>, string> =
            try
                Ok(Json.des s)
            with e ->
                Error(e.Message)

        static member TryDeserialize (id: string) (rawJson: string) =
            try
                JsonConvert.DeserializeObject<Linq.JObject> rawJson
                |> Some
            with e ->
                None
            |> Option.bind (fun jobject ->
                match jobject.TryGetValue "Id" with
                | true, v ->
                    if v.Type = Linq.JTokenType.String then
                        let currentId = (v :?> Linq.JValue).Value :?> string
                        if currentId = id then
                            try
                                jobject.ToObject<ComponentState<'ComponentId, 'Data>>()
                                |> Ok
                            with e ->
                                Error e.Message
                            |> Some
                        else
                            None
                    else
                        None
                | false, _ -> None
            )

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module ComponentState =
        let create id componentId data =
            {
                Id = id
                ComponentId = componentId
                Data = data
            }

        let header =
            "cid"

        module Printer =
            open FsharpMyExtension.ShowList

            let showEsapedString (str: string): ShowS =
                showString (str.Replace("\n", "\\\n"))

            let inline showEnum (enu: 'Enum) =
                shows (int enu)

            let inline showT (showData: _ -> ShowS) (x: ComponentState<'ComponentId, 'Data>): ShowS =
                showString header << nl
                << showEsapedString x.Id << nl
                << showEnum x.ComponentId << nl
                << showData x.Data

            let inline serialize (showData: _ -> ShowS) (x: ComponentState<'ComponentId, 'Data>): string =
                showT showData x
                |> show

        let inline serialize showData (x: ComponentState<'ComponentId, 'Data>) =
            Printer.serialize showData x

        module Parser =
            open FParsec
            open FsharpMyExtension.ResultExt
            open FsharpMyExtension.FParsecExt

            type 'a Parser = Parser<'a, unit>

            let pheader: _ Parser =
                skipString header .>> newline

            let pescapedString: _ Parser =
                let pescapedNewline =
                    pchar '\\'
                    >>. (newlineReturn "\n" <|>% "\\")

                manyStrings
                    (many1Satisfy (isNoneOf "\\\n") <|> pescapedNewline)

            // let inline pcomponentId< ^ComponentId when ^ComponentId: enum<int32>> : ^ComponentId Parser =
            //     pint32 |>> enum< ^ComponentId>

            let pcomponentId: RawComponentId Parser =
                pint32

            let pformId: FormId Parser = pescapedString

            let inline parse pdata: ComponentState<'ComponentId, 'Data> Parser =
                let p =
                    pipe3
                        (pformId .>> newline)
                        (pcomponentId .>> newline)
                        pdata
                        (fun id componentId data ->
                            {
                                Id = id
                                ComponentId = (enum<'ComponentId> componentId)
                                Data = data
                            }
                        )

                pheader >>. p

            let parseHeader str =
                run pheader str
                |> ParserResult.toResult
                |> Result.map (fun (_, _, pos) -> int pos.Index)
                |> Result.toOption

            let parseFormId (index: int) str =
                runParserOnSubstringStart (pformId .>> newline) index str
                |> ParserResult.toResult
                |> Result.map (fun (res, _, pos) -> res, int pos.Index)

            let parseComponentId (index: int) str =
                runParserOnSubstringStart (pcomponentId .>> newline) index str
                |> ParserResult.toResult
                |> Result.map (fun (res, _, pos) -> res, int pos.Index)

            let parseData (pdata: 'Data Parser) (index: int) str =
                runParserOnSubstringStart pdata index str
                |> ParserResult.toResult
                |> Result.map (fun (res, _, _) -> res)

        let inline tryDeserialize pdata str: Result<ComponentState<'ComponentId, 'Data>, _> option =
            match FParsecExt.runResult Parser.pheader str with
            | Ok _ ->
                FParsecExt.runResult (Parser.parse pdata) str
                |> Some
            | _ -> None

    [<RequireQualifiedAccessAttribute; Struct>]
    type HandleFormsError =
        | NotFoundHeader
        | ParseFormIdError of parseFormIdErrMsg: string
        | NotFoundFormId
        | ParseComponentIdError of parseComponentIdErrMsg: string
        | NotFoundComponentStateParser
        | ParseComponentStateError of parseComponentStateErrMsg: string

    type ComponentStateParser<'State> = (int32 * string) -> Result<'State, string>
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module ComponentStateParser =
        let parseMap (formId: FormId) (parseState: ComponentStateParser<'State>) map =
            let f (pos, str) =
                match parseState (pos, str) with
                | Ok (x: 'State) ->
                    Ok (map x)

                | Error(errorValue) ->
                    sprintf "%s\n%s" formId errorValue
                    |> Error

            f : ComponentStateParser<'NewState>

    type ComponentStateParsers<'State> = Map<RawComponentId, ComponentStateParser<'State>>

    module Form =
        let map (act: 'State -> 'NewState) (formId: FormId, handlers: ComponentStateParsers<'State>) : FormId * ComponentStateParsers<'NewState> =
            let handlers =
                handlers
                |> Map.map (fun _ dataParser ->
                    fun arg -> dataParser arg |> Result.map act
                )

            formId, handlers

    type Forms<'Action> = Map<FormId, ComponentStateParsers<'Action>>

    /// Parses and finds the current form from the set of forms, parses and finds the component ID,
    /// find and parses the state by the component ID, and returns the component state.
    let parseForms (forms: Forms<'State>) input =
        let parseHeader input next =
            match ComponentState.Parser.parseHeader input with
            | Some pos ->
                next pos
            | None ->
                HandleFormsError.NotFoundHeader
                |> Error

        let parseFormId (pos, input) next =
            match ComponentState.Parser.parseFormId pos input with
            | Ok (formId, pos2) ->
                (formId, pos + pos2)
                |> next

            | Error (errMsg, _, _) ->
                HandleFormsError.ParseFormIdError errMsg
                |> Error

        let getFormById formId next =
            match Map.tryFind formId forms with
            | Some form ->
                next form
            | None ->
                HandleFormsError.NotFoundFormId
                |> Error

        let parseComponentId (pos, input) next =
            match ComponentState.Parser.parseComponentId pos input with
            | Ok (componentId, pos2) ->
                (componentId, pos + pos2)
                |> next
            | Error (errMsg, _, _) ->
                HandleFormsError.ParseComponentIdError errMsg
                |> Error

        let getComponentStateParser (componentId: RawComponentId) (stateParsers: ComponentStateParsers<'State>) next =
            match Map.tryFind componentId stateParsers with
            | Some form ->
                next form
            | None ->
                HandleFormsError.NotFoundComponentStateParser
                |> Error

        let parseComponentState (parseData: ComponentStateParser<'State>) (pos, input) next =
            match parseData (pos, input) with
            | Ok x ->
                next x
            | Error errMsg ->
                HandleFormsError.ParseComponentStateError errMsg
                |> Error

        pipeBackwardBuilder {
            let! pos =
                parseHeader input

            let! formId, pos =
                parseFormId (pos, input)

            let! componentStateParsers =
                getFormById formId

            let! rawComponentId, pos =
                parseComponentId (pos, input)

            let! componentStateParser =
                getComponentStateParser rawComponentId componentStateParsers

            let! action = parseComponentState componentStateParser (pos, input)

            return Ok action
        }

    let handleForms (forms: Forms<_>) error next input =
        match parseForms forms input with
        | Error x ->
            match x with
            | HandleFormsError.NotFoundHeader ->
                false
            | HandleFormsError.NotFoundFormId ->
                false
            | HandleFormsError.ParseFormIdError errMsg ->
                error (sprintf "ParseFormId:\n%A" errMsg)
                true
            | HandleFormsError.ParseComponentIdError errMsg ->
                error (sprintf "ParseComponentId:\n%A" errMsg)
                true
            | HandleFormsError.NotFoundComponentStateParser ->
                error "NotFoundComponentStateParser"
                true
            | HandleFormsError.ParseComponentStateError errMsg ->
                error (sprintf "ParseComponentData:\n%A" errMsg)
                true
        | Ok state ->
            next state
            true

module InteractionCommand =
    type Command =
        | CommandMenu of {|
                CommandName: string
                Command: Entities.DiscordApplicationCommand
                Handler: (EventArgs.ContextMenuInteractionCreateEventArgs -> unit)
            |}
        | SlashCommand of {|
                CommandName: string
                Command: Entities.DiscordApplicationCommand
                Handler: (EventArgs.InteractionCreateEventArgs -> unit)
            |}

    type Commands = Command []
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module Commands =
        open System.Threading.Tasks

        open Types

        let register (commands: Command []) (client: DiscordClient) =
            let rawCommands =
                commands
                |> Array.map (function
                    | CommandMenu c ->
                        c.Command
                    | SlashCommand c ->
                        c.Command
                )
            client.add_Ready(Emzi0767.Utilities.AsyncEventHandler (fun client _ ->
                awaiti <| client.BulkOverwriteGlobalApplicationCommandsAsync rawCommands

                Task.CompletedTask
            ))

            let menuCommands =
                commands
                |> Array.choose (function
                    | CommandMenu c -> Some c
                    | _ -> None
                )
                |> List.ofArray

            client.add_ContextMenuInteractionCreated(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
                let isHandled =
                    menuCommands
                    |> List.exactlyFold
                        (fun st f ->
                            let st =
                                if e.Interaction.Data.Name = f.CommandName then
                                    f.Handler e
                                    true
                                else
                                    false
                            st, st
                        )
                        false

                Task.CompletedTask
            ))

            let slashCommands =
                commands
                |> Array.choose (function
                    | SlashCommand c -> Some c
                    | _ -> None
                )
                |> List.ofArray

            client.add_InteractionCreated(Emzi0767.Utilities.AsyncEventHandler (fun client e ->
                let isHandled =
                    slashCommands
                    |> List.exactlyFold
                        (fun st f ->
                            let st =
                                if e.Interaction.Data.Name = f.CommandName then
                                    f.Handler e
                                    true
                                else
                                    false
                            st, st
                        )
                        false

                Task.CompletedTask
            ))

type DataOrUrl =
    | Data of string
    | Url of string
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module DataOrUrl =
    let getOrAttachment (message: Entities.DiscordMessage) (dataOrUrl: DataOrUrl option) =
        let download url =
            let res =
                WebDownloader.tryGet id url
                |> Async.RunSynchronously
            match res with
            | Right x ->
                match x.Content with
                | WebDownloader.Text json ->
                    Ok json
                | _ ->
                    Error (sprintf "Expected WebDownloader.Text but binary.")

            | Left x ->
                Error (sprintf "%A" x)

        match dataOrUrl with
        | Some json ->
            match json with
            | Url url ->
                download url
            | Data json ->
                Ok json

        | None ->
            match Seq.tryHead message.Attachments with
            | Some jsonFile ->
                // if jsonFile.MediaType = "application/json; charset=utf-8" then
                download jsonFile.Url
            | None ->
                Error "Нужно указать настройки либо явно, либо прикрепить файл к сообщению."

    module Parser =
        open FParsec

        open DiscordMessage.Parser

        type 'a Parser = Parser<'a, unit>

        let parser: _ Parser =
            let purl =
                skipString "http"
                >>. many1Satisfy ((<>) ' ')
                |>> sprintf "http%s"

            (purl |>> Url)
            <|> (pcodeBlock <|> many1Satisfy (fun _ -> true) |>> Data)
