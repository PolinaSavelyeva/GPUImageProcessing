module Filter

open Helper
open Expecto
open CPUTools
open FilterKernels

let applyFilter = GPUTools.applyFilter clContext 64

[<Tests>]
let tests =
    testList
        "Tests"
        [ testPropertyWithConfig myConfig "Application of the generated filters on GPU is equal to the application on CPU on generated MyImage"
          <| fun myImage (kernel: Generators.Kernel) ->

              let expectedResult = applyFilterCPU kernel.Data myImage
              let actualResult = applyFilter kernel.Data myImage

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testPropertyWithConfig myConfig "Application of the filter (darken) on GPU is equal to the application on CPU on generated MyImage"
          <| fun myImage ->

              let expectedResult = applyFilterCPU darkenKernel myImage
              let actualResult = applyFilter darkenKernel myImage

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Application of the filter (gauss) on GPU is equal to the application on CPU on real image"
          <| fun _ ->

              let expectedResult = applyFilterCPU sharpenKernel myImage3
              let actualResult = applyFilter sharpenKernel myImage3

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Application of the filter (sharpen) on GPU is equal to the application on CPU on real image"
          <| fun _ ->

              let expectedResult = applyFilterCPU sharpenKernel myImage4
              let actualResult = applyFilter sharpenKernel myImage4

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Application of the filter (edges) on GPU is equal to the application on CPU on real image"
          <| fun _ ->

              let expectedResult = applyFilterCPU edgesKernel myImage4
              let actualResult = applyFilter edgesKernel myImage4

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. "

          testCase "Application of the filter (lighten) on GPU is equal to the application on CPU on real image"
          <| fun _ ->

              let expectedResult = applyFilterCPU lightenKernel myImage1
              let actualResult = applyFilter lightenKernel myImage1

              Expect.equal actualResult.Data expectedResult.Data $"Unexpected: %A{actualResult.Data}.\n Expected: %A{expectedResult.Data}. " ]
