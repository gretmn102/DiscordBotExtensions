module ExtensionsTests
open Fuchu
open FsharpMyExtension

module InteractionTests =
    open Extensions.Interaction

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
        module Form1 =
            open Extensions.Interaction

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

            type Action =
                | ConfirmMerry of FirstButtonState
                | CancelMerry of SecondButtonState

            let handlers: Map<ComponentId, string -> Result<Action, string>> =
                let f deserialize handle str =
                    match deserialize str with
                    | Ok x ->
                        Ok (handle x)

                    | Error(errorValue) ->
                        sprintf "Views.MerryConformationView.Handler.ConfirmButtonHandler\n%s" errorValue
                        |> Error

                [
                    ComponentId.FirstButton, f FirstButtonState.deserialize ConfirmMerry
                    ComponentId.SecondButton, f SecondButtonState.deserialize CancelMerry
                ]
                |> Map.ofList

        module Form2 =
            open Extensions.Interaction

            let viewId = "form2Id"

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

            type Action =
                | ConfirmMerry of FirstButtonState
                | CancelMerry of SecondButtonState

            let handlers: Map<ComponentId, _ -> Result<Action, string>> =
                let f deserialize handle str =
                    match deserialize str with
                    | Ok x ->
                        Ok (handle x)

                    | Error(errorValue) ->
                        sprintf "Views.MerryConformationView.Handler.ConfirmButtonHandler\n%s" errorValue
                        |> Error
                [
                    ComponentId.FirstButton, f FirstButtonState.deserialize ConfirmMerry
                    ComponentId.SecondButton, f SecondButtonState.deserialize CancelMerry
                ]
                |> Map.ofList

        type ViewAction =
            | Form1Action of Form1.Action
            | Form2Action of Form2.Action

        let actions =
            let inline f handlers act componentId str =
                let componentId = enum componentId
                match Map.tryFind componentId handlers with
                | Some parse ->
                    parse str
                | None ->
                    sprintf "Not found '%A' ComponentId" componentId
                    |> Error
                |> Result.map act

            [
                Form1.viewId, f Form1.handlers Form1Action
                Form2.viewId, f Form2.handlers Form2Action
            ]
            |> Map.ofList

        [<Tests>]
        let builderTests =
            let create input =
                let result = ref None
                let handleAction x =
                    result.Value <- Some (Ok x)
                let restartComponent errMsg =
                    result.Value <- Some (Error errMsg)

                let isHandled =
                    handleForms actions handleAction restartComponent input

                isHandled, result.Value

            testList "builderTests" [
                testCase "form1First" <| fun () ->
                    let act =
                        let x =
                            Form1.FirstButtonState.create "user"
                            |> Form1.FirstButtonState.serialize
                        create x

                    let exp: bool * option<Result<ViewAction,string>> =
                        (true, Some (Ok (Form1Action (Form1.ConfirmMerry { User = "user" }))))

                    Assert.Equal("", exp, act)

                testCase "form1Second" <| fun () ->
                    let act =
                        let x =
                            Form1.SecondButtonState.create 42
                            |> Form1.SecondButtonState.serialize
                        create x

                    let exp: bool * option<Result<ViewAction,string>> =
                        (true, Some (Ok (Form1Action (Form1.CancelMerry { Number = 42 }))))

                    Assert.Equal("", exp, act)

                testCase "form2First" <| fun () ->
                    let act =
                        let x =
                            Form2.FirstButtonState.create "user2"
                            |> Form2.FirstButtonState.serialize
                        create x

                    let exp: bool * option<Result<ViewAction,string>> =
                        (true, Some (Ok (Form2Action (Form2.ConfirmMerry { User = "user2" }))))

                    Assert.Equal("", exp, act)

                testCase "form2Second" <| fun () ->
                    let act =
                        let x =
                            Form2.SecondButtonState.create 24
                            |> Form2.SecondButtonState.serialize
                        create x

                    let exp: bool * option<Result<ViewAction,string>> =
                        (true, Some (Ok (Form2Action (Form2.CancelMerry { Number = 24 }))))

                    Assert.Equal("", exp, act)
            ]
