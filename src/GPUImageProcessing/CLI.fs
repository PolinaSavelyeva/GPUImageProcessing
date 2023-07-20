module CLI

open Argu
open ArguCommands
open SixLabors.ImageSharp

[<EntryPoint>]
let main argv =

    let errorHandler =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some System.ConsoleColor.DarkYellow
        )

    let parser = ArgumentParser.Create<CLIArguments>(errorHandler = errorHandler)

    match parser.ParseCommandLine argv with

    | res when
        res.Contains(InputPath)
        && res.Contains(OutputPath)
        && res.Contains(AgentsSupport)
        && res.Contains(GPUPlatform)
        && res.Contains(Transformations)
        ->

        let inputPath = res.GetResult(InputPath)
        let outputPath = res.GetResult(OutputPath)
        let processors = res.GetResult(Transformations)
        let unit = res.GetResult(GPUPlatform)
        let agentsSupport = res.GetResult(AgentsSupport)

        Processing.processImages inputPath outputPath (unit |> GPUPlatformParser) processors agentsSupport

    | _ -> printfn $"Unexpected command.\n {parser.PrintUsage()}"

    0
