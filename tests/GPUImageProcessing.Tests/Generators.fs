module Generators

open FsCheck
open System

type Kernel =
    val Data: float32[,]
    val Length: int

    new(data, length) = { Data = data; Length = length }

let generateUniqueFileName (extension: string) =

    let time = DateTime.Now.ToString("yyyyMMdd_HHmmss")
    let randomNumber = Random().Next(1000, 9999)
    let fileName = sprintf "%s_%04d.%s" time randomNumber extension

    fileName

let myImageGen =
    gen {
        let! length1 = Gen.choose (2, 100)
        let! length2 = Gen.choose (2, 100)

        let! data = Gen.arrayOfLength (length1 * length2) (Gen.elements [ 0uy .. 127uy ])

        return! Gen.constant (BasicTools.MyImage(data, length1, length2, generateUniqueFileName "jpeg"))
    }

let kernelGen =
    gen {
        let! length = Gen.elements [ 1; 3; 5 ]

        let! data = Gen.array2DOfDim (length, length) (Gen.elements [ -255f .. 255f ])
        return! Gen.constant (Kernel(data, length))
    }

type MyGenerators =
    static member MyImage() = Arb.fromGen myImageGen
    static member Kernel() = Arb.fromGen kernelGen
