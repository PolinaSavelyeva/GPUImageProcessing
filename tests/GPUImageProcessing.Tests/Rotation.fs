module Rotation

open Helper
open Expecto
open CPUTools

let rotate = GPUTools.rotate clContext 64

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "360 degree MyImage counterclockwise rotation is equal to the original on GPU"
          <| fun _ ->

              let result =
                  myImage1 |> rotate false |> rotate false |> rotate false |> rotate false

              Expect.equal result.Data myImage1.Data $"Unexpected: %A{result.Data}.\n Expected: %A{myImage1.Data}. "
              Expect.equal result.Height myImage1.Height $"Unexpected: %A{result.Height}.\n Expected: %A{myImage1.Height}. "
              Expect.equal result.Width myImage1.Width $"Unexpected: %A{result.Width}.\n Expected: %A{myImage1.Width}. "

          testCase "360 degree MyImage clockwise rotation is equal to the original on GPU"
          <| fun _ ->

              let result =
                  myImage2 |> rotate false |> rotate false |> rotate false |> rotate false

              Expect.equal result.Data myImage2.Data $"Unexpected: %A{result.Data}.\n Expected: %A{myImage2.Data}. "
              Expect.equal result.Height myImage2.Height $"Unexpected: %A{result.Height}.\n Expected: %A{myImage2.Height}. "
              Expect.equal result.Width myImage2.Width $"Unexpected: %A{result.Width}.\n Expected: %A{myImage2.Width}. "

          testPropertyWithConfig myConfig "360 degree counter/clockwise rotation is equal to the original on generated MyImage on GPU"
          <| fun myImage ->

              let resultsArray =
                  [| (myImage |> rotate true |> rotate true |> rotate true |> rotate true).Data; (myImage |> rotate false |> rotate false |> rotate false |> rotate false).Data |]

              Expect.allEqual resultsArray myImage.Data $"Unexpected: %A{resultsArray} and original {myImage.Data}.\n Expected equality. "

          testPropertyWithConfig myConfig "Clockwise/counterclockwise rotation on GPU is equal to clockwise/counterclockwise rotation on CPU on generated MyImage"
          <| fun myImage (rotation: bool) ->

              let expectedResult = rotateCPU rotation myImage
              let actualResult = rotate rotation myImage

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Clockwise rotation on GPU is equal to clockwise rotation on CPU on real image"
          <| fun _ ->

              let expectedResult = rotateCPU true myImage2
              let actualResult = rotate true myImage2

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Counterclockwise rotation on GPU is equal to Counterclockwise rotation on CPU on real image"
          <| fun _ ->

              let expectedResult = rotateCPU false myImage2
              let actualResult = rotate false myImage2

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. " ]
