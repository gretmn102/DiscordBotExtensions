module DiscordBotExtensions.EnvironmentExt
open dotenv.net

/// First function tries to read the environment variable from `.env`,
/// and if there is none, it reads from the global environment.
/// Does not throw an exception if the `.env` file is missing.
let tryGetEnvironmentVariable =
    DotEnv.Load()

    let envVars = DotEnv.Read()

    fun envVar ->
        match envVars.TryGetValue envVar with
        | false, _ ->
            match System.Environment.GetEnvironmentVariable envVar with
            | null -> None
            | value -> Some value
        | true, value -> Some value

/// First function read the environment variable from `.env`,
/// and if there is none, it reads from the global environment.
/// Does not throw an exception if the `.env` file is missing.
let getEnvironmentVariable varName =
    tryGetEnvironmentVariable varName
    |> Option.defaultWith (fun () ->
        failwithf "Environment variable `%s` is not set!" varName
    )
