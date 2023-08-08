module Rotation

open Helper
open Expecto
open CPUTools

let rotate = GPUTools.rotate clContext 64

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "360 degree counterclockwise rotated image is equal to the original on GPU"
          <| fun _ ->

              let result =
                  myImage1 |> rotate false |> rotate false |> rotate false |> rotate false

              Expect.equal result.Data myImage1.Data "The rotated image should be equal to the original."
              Expect.equal result.Height myImage1.Height "The rotated image height should match the expected new height."
              Expect.equal result.Width myImage1.Width "The rotated image width should match the expected new width."

          testCase "360 degree clockwise rotated image is equal to the original on GPU"
          <| fun _ ->

              let result =
                  myImage2 |> rotate false |> rotate false |> rotate false |> rotate false

              Expect.equal result.Data myImage2.Data "The rotated image should be equal to the original."
              Expect.equal result.Height myImage2.Height "The rotated image height should match the expected new height."
              Expect.equal result.Width myImage2.Width "The rotated image width should match the expected new width."

          testPropertyWithConfig myConfig "360 degree counter/clockwise rotated image is equal to the original on GPU"
          <| fun myImage ->

              let resultsArray =
                  [| (myImage |> rotate true |> rotate true |> rotate true |> rotate true).Data; (myImage |> rotate false |> rotate false |> rotate false |> rotate false).Data |]

              Expect.allEqual resultsArray myImage.Data "The rotated generated image should be equal to the original. "

          testPropertyWithConfig myConfig "Clockwise/counterclockwise rotated on GPU generated images is equal to clockwise/counterclockwise rotated on CPU generated images"
          <| fun myImage (rotation: bool) ->

              let expectedResult = rotateCPU rotation myImage
              let actualResult = rotate rotation myImage

              Expect.equal actualResult.Data expectedResult.Data "The GPU and CPU rotated generated image data should be equal."

          testCase "Clockwise rotation on GPU is equal to clockwise rotation on CPU on real image"
          <| fun _ ->

              let expectedResult = rotateCPU true myImage2
              let actualResult = rotate true myImage2

              Expect.equal actualResult.Data expectedResult.Data "The GPU and CPU flipped real image data should be equal."

          testCase "Counterclockwise rotation on GPU is equal to Counterclockwise rotation on CPU on real image"
          <| fun _ ->

              let expectedResult = rotateCPU false myImage2
              let actualResult = rotate false myImage2

              Expect.equal actualResult.Data expectedResult.Data "The GPU and CPU flipped real image data should be equal." ]
