module Helper

open BasicTools
open Expecto

let src = __SOURCE_DIRECTORY__
let myImage1 = load <| System.IO.Path.Combine(src, "Images", "input", "1.jpg")
let myImage2 = load <| System.IO.Path.Combine(src, "Images", "input", "2.jpg")
let myImage3 = load <| System.IO.Path.Combine(src, "Images", "input", "3.jpg")
let myImage4 = load <| System.IO.Path.Combine(src, "Images", "input", "4.jpg")

let deleteFilesInDirectory (directoryPath: string) =
    let files = System.IO.Directory.GetFiles(directoryPath)

    for file in files do
        System.IO.File.Delete(file)

let device = Brahma.FSharp.ClDevice.GetFirstAppropriateDevice()
let clContext = Brahma.FSharp.ClContext(device)

let myConfig =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.MyGenerators> ]
        maxTest = 10 }
