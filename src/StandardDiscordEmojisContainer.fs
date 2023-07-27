module DiscordBotExtensions.StandardDiscordEmojisContainer
open FsharpMyExtension
open FsharpMyExtension.Either

let emojiSheetMapWidth = 42

let emojiSheetMap =
    let filename = "StandardDiscordEmojisSheet.txt"
    let content =
        use content =
            System.Reflection.Assembly.GetExecutingAssembly()
                .GetManifestResourceStream(sprintf "DiscordBotExtensions.%s" filename)

        use stream = new System.IO.StreamReader(content)
        stream.ReadToEnd()

    content
    |> String.lines
    |> Array.mapi (fun y ->
        String.split " "
        >> Seq.mapi (fun x str -> str, (x, y))
        >> Array.ofSeq
    )
    |> Array.concat
    |> Map.ofSeq


open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing

let emojiSheetUrl = "https://discord.com/assets/2071e22f8044e1f9e4f7c7afb7ac484a.png"
let getEmojiSheet () =
    WebClientDownloader.getData [] emojiSheetUrl
    |> Either.map Image.Load

let getEmoji (emojiSheet: Image<Rgba32>) (unicodeEmoji: string) (outputStream: System.IO.MemoryStream) =
    match Map.tryFind unicodeEmoji emojiSheetMap with
    | Some (x, y) ->
        let emojiSize = emojiSheet.Width / emojiSheetMapWidth
        use emoji =
            emojiSheet.Clone(fun ctx ->
                let r = Rectangle(x * emojiSize, y * emojiSize, emojiSize, emojiSize)
                ctx.Crop r
                |> ignore
            )
        emoji.Save(outputStream, Formats.Png.PngFormat.Instance)

        true
    | None -> false
