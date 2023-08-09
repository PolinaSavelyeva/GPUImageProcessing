module Resize

open BasicTools
open Generators
open Helper
open Expecto
open CPUTools

let resize = GPUTools.resize clContext 64

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "The real bilinear-down-scaled image should have the same dimensions as declared on GPU"
          <| fun _ ->

              let newWidth = 123
              let newHeight = 293

              let result = resize newWidth newHeight GPUTools.Bilinear myImage3

              Expect.equal result.Height newHeight "The resized image height should match the expected new height."
              Expect.equal result.Width newWidth "The resized image height should match the expected new height."

          testCase "The real nearestNeighbour-down-scaled image should have the same dimensions as declared on GPU"
          <| fun _ ->

              let newWidth = 234
              let newHeight = 234

              let result = resize newWidth newHeight GPUTools.NearestNeighbour myImage3

              Expect.equal result.Height newHeight "The resized image height should match the expected new height."
              Expect.equal result.Width newWidth "The resized image width should match the expected new width."

          testCase "The real bilinear-up-scaled image should have the same dimensions as declared on GPU"
          <| fun _ ->

              let newWidth = 3234
              let newHeight = 3945

              let result = resize newWidth newHeight GPUTools.Bilinear myImage3

              Expect.equal result.Height newHeight "The resized image height should match the expected new height."
              Expect.equal result.Width newWidth "The resized image width should match the expected new width."

          testCase "The real nearestNeighbour-up-scaled image should have the same dimensions as declared on GPU"
          <| fun _ ->

              let newWidth = 4382
              let newHeight = 4192

              let result = resize newWidth newHeight GPUTools.NearestNeighbour myImage3

              Expect.equal result.Height newHeight "The resized image height should match the expected new height."
              Expect.equal result.Width newWidth "The resized image width should match the expected new width."

          testCase "The image should remain the same after nearestNeighbour-resize, where new width and new height and old ones are equal"
          <| fun _ ->

              let newWidth = myImage3.Width
              let newHeight = myImage3.Height

              let result = resize newWidth newHeight GPUTools.NearestNeighbour myImage3

              Expect.equal myImage3.Data result.Data "The resized image data should be equal to the original image data."

          testCase "The image should remain the same after bilinear-resize, where new width and new height and old ones are equal"
          <| fun _ ->

              let newWidth = myImage3.Width
              let newHeight = myImage3.Height

              let result = resize newWidth newHeight GPUTools.Bilinear myImage3

              Expect.equal myImage3.Data result.Data "The resized image data should be equal to the original image data."

          testCase "NearestNeighbour-resized real image on GPU and nearestNeighbour-resized on CPU should be equal"
          <| fun _ ->

              let newWidth = 975
              let newHeight = 632

              let expectedResult = resizeCPUNearestNeighbour newWidth newHeight myImage3
              let actualResult = resize newWidth newHeight GPUTools.NearestNeighbour myImage3

              Expect.equal actualResult.Data expectedResult.Data "The resized image data should be equal to the original image data."

          testPropertyWithConfig myConfig "NearestNeighbour-resized on generated image on GPU and nearestNeighbour-resized on CPU should be equal"
          <| fun (myImage: MyImage) (dimensions: ImageDimensions) ->

              let newWidth = dimensions.Width
              let newHeight = dimensions.Height

              let expectedResult = resizeCPUNearestNeighbour newWidth newHeight myImage
              let actualResult = resize newWidth newHeight GPUTools.NearestNeighbour myImage

              Expect.equal actualResult.Data expectedResult.Data "The resized image data should be equal to the original image data."

          testCase "Bilinear-resized real image on GPU and bilinear-resized on CPU should be equal"
          <| fun _ ->

              let newWidth = 975
              let newHeight = 632
              let tolerance = 1uy

              let expectedResult = resizeCPUBilinear newWidth newHeight myImage3
              let actualResult = resize newWidth newHeight GPUTools.Bilinear myImage3

              let areImagesEqual =
                  Array.forall2 (fun x y -> x - y <= tolerance || y - x <= tolerance) actualResult.Data expectedResult.Data

              Expect.isTrue areImagesEqual "Images are not equal within the specified tolerance."

          testPropertyWithConfig myConfig "Bilinear-resized generated image on GPU and bilinear-resized on CPU should be equal"
          <| fun (myImage: MyImage) (dimensions: ImageDimensions) ->

              let newWidth = dimensions.Width
              let newHeight = dimensions.Height
              let tolerance = 1uy

              let expectedResult = resizeCPUBilinear newWidth newHeight myImage
              let actualResult = resize newWidth newHeight GPUTools.Bilinear myImage

              let areImagesEqual =
                  Array.forall2 (fun x y -> x - y <= tolerance || y - x <= tolerance) actualResult.Data expectedResult.Data

              Expect.isTrue areImagesEqual "Images are not equal within the specified tolerance." ]
