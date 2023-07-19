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

    (*type Position =
        | TopLeft
        | TopCenter
        | TopRight
        | MiddleLeft
        | MiddleCenter
        | MiddleRight
        | BottomLeft
        | BottomCenter
        | BottomRight*)

    (*let cropCPU (image: MyImage) (x: int) (y: int) newWidth newHeight =

        let buffer = Array.create (newWidth * newHeight) 0uy

        for row = 0 to newHeight - 1 do
            for column = 0 to newWidth - 1 do

                let originalX = x + column
                let originalY = y + row

                if originalX < image.Width && originalY < image.Height then
                    buffer[row * newWidth + column] <- image.Data[originalY * image.Width + originalX]

        MyImage(buffer, newWidth, newHeight, image.Name, image.Extension)


    let image = BasicTools.load "/Users/lissa/Документы/GPUImageProcessing/tests/GPUImageProcessing.Tests/Images/input/3.jpg"
    let procImage = cropCPU image 20 10 50 50
    save image "/Users/lissa/Документы/GPUImageProcessing/tests/GPUImageProcessing.Tests/Images/output/"*)
    0
