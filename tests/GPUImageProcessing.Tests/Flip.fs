module Flip

open Helper
open Expecto

let flip = GPUTools.flip clContext 64

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "Two vertical MyImage flips is equal to the original on GPU"
          <| fun _ ->

              let result = myImage3 |> flip true |> flip true

              Expect.equal result.Data myImage3.Data $"Unexpected: %A{result.Data}.\n Expected: %A{myImage3.Data}. "
              Expect.equal result.Height myImage3.Height $"Unexpected: %A{result.Height}.\n Expected: %A{myImage3.Height}. "
              Expect.equal result.Width myImage3.Width $"Unexpected: %A{result.Width}.\n Expected: %A{myImage3.Width}. "

          testCase "Two horizontal MyImage flips is equal to the original on GPU"
          <| fun _ ->

              let result = myImage4 |> flip false |> flip false

              Expect.equal result.Data myImage4.Data $"Unexpected: %A{result.Data}.\n Expected: %A{myImage4.Data}. "
              Expect.equal result.Height myImage4.Height $"Unexpected: %A{result.Height}.\n Expected: %A{myImage4.Height}. "
              Expect.equal result.Width myImage4.Width $"Unexpected: %A{result.Width}.\n Expected: %A{myImage4.Width}. "

          testPropertyWithConfig myConfig "Two vertical/horizontal MyImage flips is equal to the original on generated MyImage on GPU"
          <| fun myImage ->

              let resultsArray =
                  [| (myImage |> flip true |> flip true).Data; (myImage |> flip false |> flip false).Data |]

              Expect.allEqual resultsArray myImage.Data $"Unexpected: %A{resultsArray} and original {myImage.Data}.\n Expected equality. "

          testPropertyWithConfig myConfig "Vertical/horizontal flip on GPU is equal to vertical/horizontal flip on CPU on generated MyImage"
          <| fun myImage (rotation: bool) ->

              let expectedResult = flipCPU rotation myImage
              let actualResult = flip rotation myImage

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Vertical flip on GPU is equal to Vertical flip on CPU on real image"
          <| fun _ ->

              let expectedResult = flipCPU true myImage3
              let actualResult = flip true myImage3

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Horizontal flip on GPU is equal to horizontal flip on CPU on real image"
          <| fun _ ->

              let expectedResult = flipCPU false myImage3
              let actualResult = flip false myImage3

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. " ]
