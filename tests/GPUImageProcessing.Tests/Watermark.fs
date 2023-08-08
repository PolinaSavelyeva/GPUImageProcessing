module Watermark

open FsCheck
open Helper
open Expecto
open BasicTools
open CPUTools

let watermark = GPUTools.watermark clContext 64

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "Watermarking on GPU is equal to watermarking on CPU on real images"
          <| fun _ ->

              let expectedResult = watermarkCPU myImage1 2f myImage3
              let actualResult = watermark myImage1 2f myImage3

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testPropertyWithConfig myConfig "Watermarking on GPU is equal to watermarking on CPU on generated image"
          <| fun (myImage: MyImage) (watermarkImage: MyImage) (scaleWatermark: NormalFloat) ->

              let scaleWatermarkFloat32 = scaleWatermark.Get |> float32

              let scaleWatermark =
                  if
                      scaleWatermarkFloat32 > 0f
                      && scaleWatermarkFloat32 > ceil (1f / float32 watermarkImage.Height)
                      && scaleWatermarkFloat32 > ceil (1f / float32 watermarkImage.Width)
                  then
                      scaleWatermarkFloat32
                  else
                      1f

              let expectedResult = watermarkCPU watermarkImage scaleWatermark myImage
              let actualResult = watermark watermarkImage scaleWatermark myImage

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Applying watermark using negative scale parameter should cause an error"
          <| fun _ ->

              Expect.throws (fun _ -> watermark myImage1 -5f myImage2 |> ignore) "Expected positive watermark scale. "

          testCase "Applying watermark using zero scale parameter should cause an error"
          <| fun _ ->

              Expect.throws (fun _ -> watermark myImage3 0f myImage2 |> ignore) "Expected positive watermark scale. "

          testCase "Applying watermark using watermark scale lower than to 1 / watermark.Height should cause an error"
          <| fun _ ->

              Expect.throws (fun _ -> watermark myImage4 0.00000001f myImage3 |> ignore) $"Expected watermark scale to be greater than or equal to %f{1f / float32 myImage4.Height}. " ]
