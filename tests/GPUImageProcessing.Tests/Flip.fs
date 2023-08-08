module Flip

open Helper
open Expecto
open CPUTools

let flip = GPUTools.flip clContext 64

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "Two times vertical-flipped image is equal to the original on GPU"
          <| fun _ ->

              let result = myImage4 |> flip true |> flip true

              Expect.equal result.Data myImage4.Data "Two times flipped image should be equal to the original."
              Expect.equal result.Height myImage4.Height "The flipped image height should match the expected new height."
              Expect.equal result.Width myImage4.Width "The flipped image width should match the expected new width."

          testCase "Two times horizontal-flipped image is equal to the original on GPU"
          <| fun _ ->

              let result = myImage2 |> flip false |> flip false

              Expect.equal result.Data myImage2.Data "Two times flipped image should be equal to the original."
              Expect.equal result.Height myImage2.Height "The flipped image height should match the expected new height."
              Expect.equal result.Width myImage2.Width "The flipped image width should match the expected new width."

          testPropertyWithConfig myConfig "Two times horizontal/vertical-flipped image is equal to the original on GPU"
          <| fun myImage ->

              let resultsArray =
                  [| (myImage |> flip true |> flip true).Data; (myImage |> flip false |> flip false).Data |]

              Expect.allEqual resultsArray myImage.Data "Two times flipped image should be equal to the original."

          testPropertyWithConfig myConfig "Vertical/horizontal flip on GPU is equal to vertical/horizontal flip on CPU on generated MyImage"
          <| fun myImage (rotation: bool) ->

              let expectedResult = flipCPU rotation myImage
              let actualResult = flip rotation myImage

              Expect.equal actualResult.Data expectedResult.Data "The GPU and CPU flipped generated image data should be equal."

          testCase "Vertical flip on GPU is equal to Vertical flip on CPU on real image"
          <| fun _ ->

              let expectedResult = flipCPU true myImage3
              let actualResult = flip true myImage3

              Expect.equal actualResult.Data expectedResult.Data "The GPU and CPU flipped real image data should be equal."

          testCase "Horizontal flip on GPU is equal to horizontal flip on CPU on real image"
          <| fun _ ->

              let expectedResult = flipCPU false myImage3
              let actualResult = flip false myImage3

              Expect.equal actualResult.Data expectedResult.Data "The GPU and CPU flipped real image data should be equal." ]
