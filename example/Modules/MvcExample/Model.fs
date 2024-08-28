module MvcExample.Model
open FsharpMyExtension
open DiscordBotExtensions.Mvc.Model

[<RequireQualifiedAccessAttribute;Struct>]
type ViewCmd =
    | Reconnect
    | Hello of string

[<RequireQualifiedAccessAttribute>]
type MyCmd =
    | MvcCmd of Cmd<ViewCmd, MyCmd>
    /// custom command
    | Reconnect of Req<unit, unit, MyCmd>
    | End

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MyCmd =
    let mvcCmd fn next =
        MyCmd.MvcCmd (fn (fun res ->
            next res
        ))

    let reconnect arg next =
        MyCmd.Reconnect(arg, next)

let reload () =
    pipeBackwardBuilder {
        let! _ = MyCmd.mvcCmd (Cmd.responseCreateView true ViewCmd.Reconnect)
        do! MyCmd.reconnect ()
        return MyCmd.End
    }

let hello name =
    pipeBackwardBuilder {
        let! messageId = MyCmd.mvcCmd (Cmd.responseCreateView false (ViewCmd.Hello name))
        match messageId with
        | Some messageId ->
            let! _ =
                MyCmd.mvcCmd (Cmd.setTimeout 1000 (
                    pipeBackwardBuilder {
                        do! MyCmd.mvcCmd (Cmd.removeCurrentView None)
                        return MyCmd.End
                    })
                )
            return MyCmd.End
        | None ->
            return MyCmd.End
    }
