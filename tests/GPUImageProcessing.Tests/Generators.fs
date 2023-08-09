module Generators

open FsCheck
open System

let generateUniqueFileName (extension: string) =

    let time = DateTime.Now.ToString("yyyyMMdd_HHmmss")
    let randomNumber = Random().Next(1000, 9999)
    let fileName = sprintf "%s_%04d.%s" time randomNumber extension

    fileName

let myImageGen =
    gen {
        let! length1 = Gen.choose (2, 200)
        let! length2 = Gen.choose (2, 200)

        let! data = Gen.arrayOfLength (length1 * length2) (Gen.elements [ 0uy .. 127uy ])

        return! Gen.constant (BasicTools.MyImage(data, length1, length2, generateUniqueFileName "jpeg"))
    }

type Kernel =
    val Data: float32[,]
    val Length: int

    new(data, length) = { Data = data; Length = length }

let kernelGen =
    gen {
        let! length = Gen.elements [ 1; 3; 5 ]

        let! data = Gen.array2DOfDim (length, length) (Gen.elements [ -255f .. 255f ])

        return! Gen.constant (Kernel(data, length))
    }

type ImageDimensions =
    val Height: int
    val Width: int

    new(height, width) = { Height = height; Width = width }

let imageDimensionsGen =
    gen {
        let! height = Gen.choose (1, 200)
        let! width = Gen.choose (1, 200)

        return! Gen.constant (ImageDimensions(height, width))
    }

type ImageCroppingCoordinates =
    val XUpper: int
    val YUpper: int
    val XLower: int
    val YLower: int

    new(xUpper, yUpper, xLower, yLower) =
        { XUpper = xUpper
          YUpper = yUpper
          XLower = xLower
          YLower = yLower }

let imageCroppingCoordinatesGen =

    let upperChooser x1 x2 = if x1 = x2 then x1 - 1 else min x1 x2

    gen {
        let! height1 = Gen.choose (1, 200)
        let! height2 = Gen.choose (1, 200)

        let! width1 = Gen.choose (1, 200)
        let! width2 = Gen.choose (1, 200)

        return! Gen.constant (ImageCroppingCoordinates(upperChooser width1 width2, upperChooser height1 height2, max width1 width2, max height1 height2))
    }

type MyGenerators =
    static member GeneratedMyImage() = Arb.fromGen myImageGen
    static member GeneratedKernel() = Arb.fromGen kernelGen
    static member GeneratedDimension() = Arb.fromGen imageDimensionsGen
    static member GeneratedCroppingCoordinates() = Arb.fromGen imageCroppingCoordinatesGen
