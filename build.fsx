// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let f projName =
    let pattern = sprintf @"**/%s.fsproj" projName
    let xs = !! pattern
    xs
    |> Seq.tryExactlyOne
    |> Option.defaultWith (fun () ->
        xs
        |> List.ofSeq
        |> failwithf "'%s' expected exactly one but:\n%A" pattern
    )

let testProjName = "Tests"
let testProjPath = f testProjName
let testsProjDir = Path.getDirectory testProjPath
let mainProjName = "DiscordBotExtensions"
let mainProjPath = f mainProjName
let mainProjDir = Path.getDirectory mainProjPath
let exampleBotName = "ExampleBot"
let exampleBotPath = f exampleBotName
let exampleBotDir = Path.getDirectory exampleBotPath


let deployDir = Path.getFullName "./deploy"

let release = ReleaseNotes.load "RELEASE_NOTES.md"
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

module XmlText =
    let escape rawText =
        let doc = new System.Xml.XmlDocument()
        let node = doc.CreateElement("root")
        node.InnerText <- rawText
        node.InnerXml
// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
let cleanBinAndObj projectPath =
    Shell.cleanDirs [
        projectPath </> "bin"
        projectPath </> "obj"
    ]

Target.create "MainClean" (fun _ ->
    cleanBinAndObj mainProjDir
)

Target.create "TestsClean" (fun _ ->
    cleanBinAndObj testsProjDir
)

Target.create "ExampleBotClean" (fun _ ->
    cleanBinAndObj exampleBotDir
)

Target.create "DeployClean" (fun _j ->
    Shell.cleanDir deployDir
)

Target.create "Clean" ignore

Target.create "Meta" (fun _ ->
    [
        "<Project xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
        "<ItemGroup>"
        "  <PackageReference Include=\"Microsoft.SourceLink.GitHub\" Version=\"1.0.0\" PrivateAssets=\"All\"/>"
        "</ItemGroup>"
        "<PropertyGroup>"
        "  <EmbedUntrackedSources>true</EmbedUntrackedSources>"
        "  <PackageProjectUrl>https://github.com/gretmn102/DiscordBotExtensions</PackageProjectUrl>"
        "  <PackageLicenseExpression>MIT</PackageLicenseExpression>"
        "  <RepositoryUrl>https://github.com/gretmn102/DiscordBotExtensions.git</RepositoryUrl>"
        sprintf "  <PackageReleaseNotes>%s</PackageReleaseNotes>"
            (String.concat "\n" release.Notes |> XmlText.escape)
        "  <PackageTags>discord;fsharp</PackageTags>"
        "  <Authors>Fering</Authors>"
        sprintf "  <Version>%s</Version>" (string release.SemVer)
        "</PropertyGroup>"
        "</Project>"
    ]
    |> File.write false "Directory.Build.props"
)

Target.create "Build" (fun _ ->
    mainProjDir
    |> dotnet "build -c Release"
)

Target.create "Deploy" (fun _ ->
    mainProjDir
    |> dotnet (sprintf "build -c Release -o \"%s\"" deployDir)
)

Target.create "Pack" (fun _ ->
    mainProjDir
    |> dotnet (sprintf "pack -c Release -o \"%s\"" deployDir)
)

Target.create "PublishToGitlab" (fun _ ->
    let packPathPattern = sprintf "%s/*.nupkg" deployDir
    let packPath =
        !! packPathPattern |> Seq.tryExactlyOne
        |> Option.defaultWith (fun () -> failwithf "'%s' not found" packPathPattern)

    deployDir
    |> dotnet (sprintf "nuget push -s %s %s" "gitlab" packPath)
)

Target.create "BuildTests" (fun _ ->
    testsProjDir
    |> dotnet "build -c Release"
)

Target.create "RunTests" (fun _ ->
    testsProjDir
    |> dotnet "run -c Release"
)

Target.create "RunExampleBot" (fun _ ->
    exampleBotDir
    |> dotnet "run -c Release"
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"Build"

"MainClean" ==> "Clean"
"TestsClean" ==> "Clean"
"DeployClean" ==> "Clean"
"ExampleBotClean" ==> "Clean"

"Clean" ?=> "Deploy"

"Clean"
  ==> "Meta"
  ==> "Pack"
  ==> "PublishToGitlab"

"BuildTests"

"RunTests"

"RunExampleBot"

Target.runOrDefault "Deploy"
