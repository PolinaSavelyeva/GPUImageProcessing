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

              Expect.equal result.Height newHeight $"Unexpected: %A{result.Height}.\n Expected: %A{newHeight}. "
              Expect.equal result.Width newWidth $"Unexpected: %A{result.Width}.\n Expected: %A{newWidth}. "

          testCase "The real nearestNeighbour-down-scaled image should have the same dimensions as declared on GPU"
          <| fun _ ->

              let newWidth = 234
              let newHeight = 234

              let result = resize newWidth newHeight GPUTools.NearestNeighbour myImage3

              Expect.equal result.Height newHeight $"Unexpected: %A{result.Height}.\n Expected: %A{newHeight}. "
              Expect.equal result.Width newWidth $"Unexpected: %A{result.Width}.\n Expected: %A{newWidth}. "

          testCase "The real bilinear-up-scaled image should have the same dimensions as declared on GPU"
          <| fun _ ->

              let newWidth = 3234
              let newHeight = 3945

              let result = resize newWidth newHeight GPUTools.Bilinear myImage3

              Expect.equal result.Height newHeight $"Unexpected: %A{result.Height}.\n Expected: %A{newHeight}. "
              Expect.equal result.Width newWidth $"Unexpected: %A{result.Width}.\n Expected: %A{newWidth}. "

          testCase "The real nearestNeighbour-up-scaled image should have the same dimensions as declared on GPU"
          <| fun _ ->

              let newWidth = 4382
              let newHeight = 4192

              let result = resize newWidth newHeight GPUTools.NearestNeighbour myImage3

              Expect.equal result.Height newHeight $"Unexpected: %A{result.Height}.\n Expected: %A{newHeight}. "
              Expect.equal result.Width newWidth $"Unexpected: %A{result.Width}.\n Expected: %A{newWidth}. "

          testCase "The image should remain the same after nearestNeighbour-resize, where new width and new height and old ones are equal"
          <| fun _ ->

              let newWidth = myImage3.Width
              let newHeight = myImage3.Height

              let result = resize newWidth newHeight GPUTools.NearestNeighbour myImage3

              Expect.equal myImage3.Data result.Data $"Unexpected: %A{myImage3.Data}.\n Expected: %A{result.Data}. "

          testCase "The image should remain the same after bilinear-resize, where new width and new height and old ones are equal"
          <| fun _ ->

              let newWidth = myImage3.Width
              let newHeight = myImage3.Height

              let result = resize newWidth newHeight GPUTools.Bilinear myImage3

              Expect.equal myImage3.Data result.Data $"Unexpected: %A{myImage3.Data}.\n Expected: %A{result.Data}. "

          testCase "NearestNeighbour-resized real image on GPU and nearestNeighbour-resized on CPU should be equal"
          <| fun _ ->

              let newWidth = 975
              let newHeight = 632

              let expectedResult = resizeCPUNearestNeighbour newWidth newHeight myImage3
              let actualResult = resize newWidth newHeight GPUTools.NearestNeighbour myImage3

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testPropertyWithConfig myConfig "NearestNeighbour-resized on generated image on GPU and nearestNeighbour-resized on CPU should be equal"
          <| fun (myImage: MyImage) (dimensions: ImageDimensions) ->

              let newWidth = dimensions.Width
              let newHeight = dimensions.Height

              let expectedResult = resizeCPUNearestNeighbour newWidth newHeight myImage
              let actualResult = resize newWidth newHeight GPUTools.NearestNeighbour myImage

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Bilinear-resized real image on GPU and bilinear-resized on CPU should be equal"
          <| fun _ ->

              let newWidth = 975
              let newHeight = 632

              let expectedResult = resizeCPUBilinear newWidth newHeight myImage3
              let actualResult = resize newWidth newHeight GPUTools.Bilinear myImage3

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testPropertyWithConfig myConfig "Bilinear-resized generated image on GPU and bilinear-resized on CPU should be equal"
          <| fun (myImage: MyImage) (dimensions: ImageDimensions) ->

              let newWidth = dimensions.Width
              let newHeight = dimensions.Height

              let expectedResult = resizeCPUBilinear newWidth newHeight myImage
              let actualResult = resize newWidth newHeight GPUTools.Bilinear myImage

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. " ]
