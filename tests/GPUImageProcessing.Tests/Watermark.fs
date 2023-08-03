module Watermark

(*open Helper
open Expecto
open Generators
open BasicTools
open CPUTools

let watermark = GPUTools.watermark clContext 64

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "Watermarking on GPU is equal to watermarking on CPU on real images"
          <| fun _ ->

              let expectedResult = watermarkCPU myImage1 2 myImage3
              let actualResult = watermark myImage1 2 myImage3

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testPropertyWithConfig myConfig "Watermarking on GPU is equal to watermarking on CPU on generated image"
          <| fun (myImage: MyImage) (watermarkImage: MyImage) (scaleWatermark: uint) ->

              let scaleWatermark =
                  if scaleWatermark = 0u then
                      4
                  else
                      System.Convert.ToInt32 scaleWatermark

              let expectedResult = watermarkCPU watermarkImage scaleWatermark myImage
              let actualResult = watermark watermarkImage scaleWatermark myImage

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Applying watermark using negative scale parameter should cause an error"
          <| fun _ ->

              Expect.throws (fun _ -> watermark myImage1 -5 myImage2 |> ignore) "Expected positive watermark scale. "

          testCase "Applying watermark using zero scale parameter should cause an error"
          <| fun _ ->

              Expect.throws (fun _ -> watermark myImage1 0 myImage1 |> ignore) "Expected positive watermark scale. " ]
*)
