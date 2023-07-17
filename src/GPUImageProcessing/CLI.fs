module CLI

open Argu
open ArguCommands

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

        Process.processImages
            inputPath
            outputPath
            (unit
             |> arguGPUPlatformParser)
            processors
            agentsSupport

    | _ -> printfn $"Unexpected command.\n {parser.PrintUsage()}"

    0
