module ArguCommands

open Argu

type ArguGPUPlatform =
    | NvidiaGPU
    | IntelGPU
    | AmdGPU
    | AnyGPU

let arguGPUPlatformParser unit =
    match unit with
    | NvidiaGPU -> Brahma.FSharp.Platform.Nvidia
    | IntelGPU -> Brahma.FSharp.Platform.Intel
    | AmdGPU -> Brahma.FSharp.Platform.Amd
    | AnyGPU -> Brahma.FSharp.Platform.Any

type CLIArguments =
    | [<Mandatory; AltCommandLine("-in")>] InputPath of inputPath: string
    | [<Mandatory; AltCommandLine("-out")>] OutputPath of outputPath: string
    | [<AltCommandLine("-agent"); EqualsAssignment>] AgentsSupport of Process.AgentsSupport
    | [<AltCommandLine("-unit"); EqualsAssignment>] GPUPlatform of ArguGPUPlatform
    | [<Mandatory; MainCommand>] Transformations of list<Process.Transformations>

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | InputPath _ -> "Path to a file or a directory where the images will be processed from."
            | OutputPath _ -> "Path to a file or a directory where the images will be saved."
            | AgentsSupport _ -> "Process files using different agents strategy."
            | GPUPlatform _ -> "Process files using specific GPU platform."
            | Transformations _ -> "List of available transformations."
