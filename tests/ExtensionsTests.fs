module ExtensionsTests
open Fuchu
open FsharpMyExtension

open DiscordBotExtensions

module InteractionTests =
    open DiscordBotExtensions.Extensions.Interaction

    module SerializeTests =
        type ComponentId =
            | List = 0

        type ComponentState = ComponentState<ComponentId, string>

        [<Tests>]
        let componentStateSerializationTests =
            testList "componentStateSerializationTests" [
                testCase "base" <| fun () ->
                    let exp: ComponentState =
                        ComponentState.create "comp\\\n1\nz" ComponentId.List "12\\1\\n3\\\\4"

                    let act =
                        exp
                        |> ComponentState.serialize ComponentState.Printer.showEsapedString
                        |> ComponentState.tryDeserialize ComponentState.Parser.pescapedString

                    Assert.Equal("", Some (Ok exp), act)
            ]

    module FormsHandleTests =
        module FirstModule =
            module Form1 =
                open DiscordBotExtensions.Extensions.Interaction

                let viewId = "form1Id"

                type ComponentId =
                    | FirstButton = 0
                    | SecondButton = 1

                type FirstButtonState =
                    {
                        User: string
                    }
                [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
                [<RequireQualifiedAccess>]
                module FirstButtonState =
                    let create user = { User = user }
                    let serialize x =
                        let ser (x: FirstButtonState) =
                            let raw = Json.ser x
                            ShowList.showString raw

                        x
                        |> ComponentState.create
                            viewId
                            ComponentId.FirstButton
                        |> ComponentState.serialize ser

                    let deserialize: string -> Result<FirstButtonState, string> =
                        Json.tryDes

                type SecondButtonState =
                    {
                        Number: int
                    }
                [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
                [<RequireQualifiedAccess>]
                module SecondButtonState =
                    let create num = { Number = num }
                    let serialize (x: SecondButtonState) =
                        let ser (x: SecondButtonState) =
                            let raw = Json.ser x
                            ShowList.showString raw

                        x
                        |> ComponentState.create
                            viewId
                            ComponentId.SecondButton
                        |> ComponentState.serialize ser

                    let deserialize: string -> Result<SecondButtonState, string> =
                        Json.tryDes

                [<RequireQualifiedAccess>]
                type ComponentState =
                    | FirstButton of FirstButtonState
                    | SecondButton of SecondButtonState

                let handler: FormId * ComponentStateParsers<ComponentState> =
                    let handlers: Extensions.Interaction.ComponentStateParsers<ComponentState> =
                        let parse parseState map =
                            let parseState (pos, str: string) =
                                parseState str.[pos..]

                            ComponentStateParser.parseMap viewId parseState map

                        [
                            int ComponentId.FirstButton, parse FirstButtonState.deserialize ComponentState.FirstButton
                            int ComponentId.SecondButton, parse SecondButtonState.deserialize ComponentState.SecondButton
                        ]
                        |> Map.ofList

                    viewId, handlers

            module Form2 =
                open Extensions.Interaction

                let viewId: FormId = "form2Id"

                type ComponentId =
                    | FirstButton = 0
                    | SecondButton = 1

                type FirstButtonState =
                    {
                        User: string
                    }
                [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
                [<RequireQualifiedAccess>]
                module FirstButtonState =
                    let create user = { User = user }
                    let serialize (x: FirstButtonState) =
                        let ser x =
                            let raw = Json.ser x
                            ShowList.showString raw

                        x
                        |> ComponentState.create
                            viewId
                            ComponentId.FirstButton
                        |> ComponentState.serialize ser

                    let deserialize = Json.tryDes

                type SecondButtonState =
                    {
                        Number: int
                    }
                [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
                [<RequireQualifiedAccess>]
                module SecondButtonState =
                    let create num = { Number = num }
                    let serialize (x: SecondButtonState) =
                        let ser x =
                            let raw = Json.ser x
                            ShowList.showString raw

                        x
                        |> ComponentState.create
                            viewId
                            ComponentId.SecondButton
                        |> ComponentState.serialize ser

                    let deserialize = Json.tryDes

                [<RequireQualifiedAccessAttribute>]
                type ComponentState =
                    | FirstButton of FirstButtonState
                    | SecondButton of SecondButtonState

                let handler: FormId * ComponentStateParsers<ComponentState> =
                    let handlers: ComponentStateParsers<ComponentState> =
                        let parse parseState map =
                            let parseState (pos, str: string) =
                                parseState str.[pos..]

                            ComponentStateParser.parseMap viewId parseState map

                        [
                            int ComponentId.FirstButton, parse FirstButtonState.deserialize ComponentState.FirstButton
                            int ComponentId.SecondButton, parse SecondButtonState.deserialize ComponentState.SecondButton
                        ]
                        |> Map.ofList

                    viewId, handlers

            [<RequireQualifiedAccessAttribute>]
            type FormComponentState =
                | Form1 of Form1.ComponentState
                | Form2 of Form2.ComponentState

            let formsComponentState: Forms<FormComponentState> =
                [
                    Form.map FormComponentState.Form1 Form1.handler
                    Form.map FormComponentState.Form2 Form2.handler
                ]
                |> Map.ofList

        module SecondModule =
            module Form3 =
                open Extensions.Interaction

                let viewId: FormId = "form3Id"

                type ComponentId =
                    | FirstButton = 0

                type FirstButtonState =
                    {
                        Number: int
                    }
                [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
                [<RequireQualifiedAccess>]
                module FirstButtonState =
                    let create num = { Number = num }
                    let serialize (x: FirstButtonState) =
                        let ser x =
                            let raw = Json.ser x
                            ShowList.showString raw

                        x
                        |> ComponentState.create
                            viewId
                            ComponentId.FirstButton
                        |> ComponentState.serialize ser

                    let deserialize = Json.tryDes

                [<RequireQualifiedAccessAttribute>]
                type ComponentState =
                    | FirstButtonState of FirstButtonState

                let handler: FormId * ComponentStateParsers<ComponentState> =
                    let handlers: ComponentStateParsers<ComponentState> =
                        let parse parseState map =
                            let parseState (pos, str: string) =
                                parseState str.[pos..]

                            ComponentStateParser.parseMap viewId parseState map

                        [
                            int ComponentId.FirstButton, parse FirstButtonState.deserialize ComponentState.FirstButtonState
                        ]
                        |> Map.ofList

                    viewId, handlers

            [<RequireQualifiedAccessAttribute>]
            type FormComponentState =
                | Form3State of Form3.ComponentState

            let formsComponentState: Forms<FormComponentState> =
                [
                    Form.map FormComponentState.Form3State Form3.handler
                ]
                |> Map.ofList

        [<Tests>]
        let builderTests =
            let create moduleForms input =
                let result = ref None
                let handleAction x =
                    result.Value <- Some (Ok x)
                let restartComponent errMsg =
                    result.Value <- Some (Error errMsg)

                let isHandled =
                    handleForms moduleForms restartComponent handleAction input

                isHandled, result.Value

            testList "builderTests" [
                testCase "form1First" <| fun () ->
                    let act =
                        let x =
                            FirstModule.Form1.FirstButtonState.create "user"
                            |> FirstModule.Form1.FirstButtonState.serialize
                        create FirstModule.formsComponentState x

                    let exp: bool * option<Result<_,string>> =
                        (true, Some (Ok (FirstModule.FormComponentState.Form1 (FirstModule.Form1.ComponentState.FirstButton { User = "user" }))))

                    Assert.Equal("", exp, act)

                testCase "form1Second" <| fun () ->
                    let act =
                        let x =
                            FirstModule.Form1.SecondButtonState.create 42
                            |> FirstModule.Form1.SecondButtonState.serialize
                        create FirstModule.formsComponentState x

                    let exp: bool * option<Result<_,string>> =
                        (true, Some (Ok (FirstModule.FormComponentState.Form1 (FirstModule.Form1.ComponentState.SecondButton { Number = 42 }))))

                    Assert.Equal("", exp, act)

                testCase "form2First" <| fun () ->
                    let act =
                        let x =
                            FirstModule.Form2.FirstButtonState.create "user2"
                            |> FirstModule.Form2.FirstButtonState.serialize
                        create FirstModule.formsComponentState x

                    let exp: bool * option<Result<_,string>> =
                        (true, Some (Ok (FirstModule.FormComponentState.Form2 (FirstModule.Form2.ComponentState.FirstButton { User = "user2" }))))

                    Assert.Equal("", exp, act)

                testCase "form2Second" <| fun () ->
                    let act =
                        let x =
                            FirstModule.Form2.SecondButtonState.create 24
                            |> FirstModule.Form2.SecondButtonState.serialize
                        create FirstModule.formsComponentState x

                    let exp: bool * option<Result<_,string>> =
                        (true, Some (Ok (FirstModule.FormComponentState.Form2 (FirstModule.Form2.ComponentState.SecondButton { Number = 24 }))))

                    Assert.Equal("", exp, act)

                testCase "form3Fail" <| fun () ->
                    let act =
                        let x =
                            FirstModule.Form1.SecondButtonState.create 24
                            |> FirstModule.Form1.SecondButtonState.serialize
                        create SecondModule.formsComponentState x

                    let exp: bool * option<Result<_,string>> =
                        (false, None)

                    Assert.Equal("", exp, act)

                testCase "form3First" <| fun () ->
                    let act =
                        let x =
                            SecondModule.Form3.FirstButtonState.create 24
                            |> SecondModule.Form3.FirstButtonState.serialize
                        create SecondModule.formsComponentState x

                    let exp: bool * option<Result<_,string>> =
                        (true, Some (Ok (SecondModule.FormComponentState.Form3State (SecondModule.Form3.ComponentState.FirstButtonState { Number = 24 }))))

                    Assert.Equal("", exp, act)
            ]
