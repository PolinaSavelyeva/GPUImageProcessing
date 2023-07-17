module ArguCommands

open Argu

/// <summary>
/// Represents the available GPU platforms for image processing.
/// </summary>
type GPUPlatform =
    | Nvidia
    | Intel
    | Amd
    | Any

let GPUPlatformParser unit =
    match unit with
    | Nvidia -> Brahma.FSharp.Platform.Nvidia
    | Intel -> Brahma.FSharp.Platform.Intel
    | Amd -> Brahma.FSharp.Platform.Amd
    | Any -> Brahma.FSharp.Platform.Any

type CLIArguments =
    | [<Mandatory; AltCommandLine("-in")>] InputPath of inputPath: string
    | [<Mandatory; AltCommandLine("-out")>] OutputPath of outputPath: string
    | [<AltCommandLine("-agent"); EqualsAssignment>] AgentsSupport of Processing.AgentsSupport
    | [<AltCommandLine("-unit"); EqualsAssignment>] GPUPlatform of GPUPlatform
    | [<Mandatory; MainCommand>] Transformations of list<Processing.Transformations>

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | InputPath _ -> "Path to a file or a directory where the images will be processed from."
            | OutputPath _ -> "Path to a file or a directory where the images will be saved."
            | AgentsSupport _ -> "Process files using different agents strategy."
            | GPUPlatform _ -> "Process files using specific GPU platform."
            | Transformations _ -> "List of available transformations."
