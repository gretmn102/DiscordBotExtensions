module MvcExample.View
open DSharpPlus

module ReloadView =
    let create () =
        let b = Entities.DiscordMessageBuilder()
        b.Content <- "reloading..."
        b

module HelloView =
    let create name =
        let b = Entities.DiscordMessageBuilder()
        b.Content <- sprintf "Hello, %s" name
        b
