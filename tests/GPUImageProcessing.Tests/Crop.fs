module Crop

open BasicTools
open Helper
open Expecto
open Generators

let crop = GPUTools.crop clContext 64

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "Cropping real image at coordinates (0,0) and (original width - 1, original height - 1) should yield the original one"
          <| fun _ ->

              let result = myImage1 |> crop (0, 0) (myImage1.Width - 1, myImage1.Height - 1)

              Expect.equal result.Data myImage1.Data $"Unexpected: %A{result.Data}.\n Expected: %A{myImage1.Data}. "
              Expect.equal result.Height myImage1.Height $"Unexpected: %A{result.Height}.\n Expected: %A{myImage1.Height}. "
              Expect.equal result.Width myImage1.Width $"Unexpected: %A{result.Width}.\n Expected: %A{myImage1.Width}. "

          testPropertyWithConfig myConfig "Cropping generated image at coordinates (0,0) and (original width - 1, original height - 1) should yield the original one"
          <| fun myImage ->

              let result = myImage |> crop (0, 0) (myImage.Width - 1, myImage.Height - 1)

              Expect.equal result.Data myImage.Data $"Unexpected: %A{result.Data}.\n Expected: %A{myImage.Data}. "
              Expect.equal result.Height myImage.Height $"Unexpected: %A{result.Height}.\n Expected: %A{myImage.Height}. "
              Expect.equal result.Width myImage.Width $"Unexpected: %A{result.Width}.\n Expected: %A{myImage.Width}. "

          testCase "Cropping real image on GPU is equal to cropping it on CPU"
          <| fun _ ->

              let xUpper, yUpper = 123, 300
              let xLower, yLower = 125, 350

              let expectedResult = cropCPU myImage3 (xUpper, yUpper) (xLower, yLower)
              let actualResult = crop (xUpper, yUpper) (xLower, yLower) myImage3

              Expect.equal expectedResult.Data actualResult.Data $"Unexpected: %A{expectedResult.Data}.\n Expected: %A{actualResult.Data}. "
              Expect.equal expectedResult.Height actualResult.Height $"Unexpected: %A{expectedResult.Height}.\n Expected: %A{actualResult.Height}. "
              Expect.equal expectedResult.Data actualResult.Data $"Unexpected: %A{expectedResult.Data}.\n Expected: %A{actualResult.Data}. "

          testPropertyWithConfig myConfig "Cropping generated image on GPU is equal to cropping it on CPU"
          <| fun (myImage: MyImage) (coordinates: ImageCroppingCoordinates) ->

              let xUpper, yUpper = coordinates.XUpper, coordinates.YUpper
              let xLower, yLower = coordinates.XLower, coordinates.YLower

              if xLower > myImage.Width || yLower > myImage.Height then

                  Expect.throws (fun _ -> cropCPU myImage (xUpper, yUpper) (xLower, yLower) |> ignore) "Corner points coordinates are out of the image."
                  Expect.throws (fun _ -> crop (xUpper, yUpper) (xLower, yLower) myImage |> ignore) "Corner points coordinates are out of the image."
              else

                  let expectedResult = cropCPU myImage (xUpper, yUpper) (xLower, yLower)
                  let actualResult = crop (xUpper, yUpper) (xLower, yLower) myImage

                  Expect.equal expectedResult.Data actualResult.Data $"Unexpected: %A{expectedResult.Data}.\n Expected: %A{actualResult.Data}. "
                  Expect.equal expectedResult.Height actualResult.Height $"Unexpected: %A{expectedResult.Height}.\n Expected: %A{actualResult.Height}. "
                  Expect.equal expectedResult.Data actualResult.Data $"Unexpected: %A{expectedResult.Data}.\n Expected: %A{actualResult.Data}. "

          testCase "Cropping by coordinates outside the image (yLower) should cause an error"
          <| fun _ ->

              let xUpper, yUpper = 123, 441
              let xLower, yLower = 300, myImage3.Height

              Expect.throws (fun _ -> crop (xUpper, yUpper) (xLower, yLower) myImage3 |> ignore) "Corner points coordinates are out of the image."

          testCase "Cropping by coordinates outside the image (xLower) should cause an error"
          <| fun _ ->

              let xUpper, yUpper = 123, 441
              let xLower, yLower = myImage3.Width, 450

              Expect.throws (fun _ -> crop (xUpper, yUpper) (xLower, yLower) myImage3 |> ignore) "Corner points coordinates are out of the image." ]
