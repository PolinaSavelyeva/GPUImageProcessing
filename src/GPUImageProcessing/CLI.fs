module CLI

open Argu
open ArguCommands
open SixLabors.ImageSharp
open BasicTools

[<EntryPoint>]
let main argv =

    (*let errorHandler =
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

    | _ -> printfn $"Unexpected command.\n {parser.PrintUsage()}"*)

    (*let image = load "/Users/lissa/Документы/GPUImageProcessing/tests/GPUImageProcessing.Tests/Images/input/2.jpg"

    printf $"%A{image.Width} %A{image.Height}"

    let procImageCPU = cropCPU image (1000,0) (3000,4000)

    let device = Brahma.FSharp.ClDevice.GetFirstAppropriateDevice()
    let clContext = Brahma.FSharp.ClContext(device)

    let procImageGPU = GPUTools.crop clContext 64 (1000,0) (3000,4000) image

    printf $"%A{procImageCPU.Data = procImageGPU.Data}"

    //procImageGPU.ChangeName "test1GPU"
    //procImageCPU.ChangeName "test1CPU"

    //save procImageCPU "/Users/lissa/Документы/GPUImageProcessing/tests/GPUImageProcessing.Tests/Images/output/"
    //save procImageGPU "/Users/lissa/Документы/GPUImageProcessing/tests/GPUImageProcessing.Tests/Images/output/"*)


    0
