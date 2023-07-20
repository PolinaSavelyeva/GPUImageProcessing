module Helper

open BasicTools
open Expecto

let src = __SOURCE_DIRECTORY__
let myImage1 = load (src + "/Images/input/1.jpg")
let myImage2 = load (src + "/Images/input/2.jpg")
let myImage3 = load (src + "/Images/input/3.jpg")
let myImage4 = load (src + "/Images/input/4.jpg")

let device = Brahma.FSharp.ClDevice.GetFirstAppropriateDevice()
let clContext = Brahma.FSharp.ClContext(device)

let myConfig =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.MyGenerators> ]
        maxTest = 10 }

let applyFilterCPU filter (image: MyImage) =

    let filterDiameter = (Array2D.length1 filter) / 2

    let filter = Helper.toFlatArray filter

    let pixelProcessing p =

        let pw = p % image.Width

        let ph = p / image.Width

        let dataToHandle =
            [| for i in ph - filterDiameter .. ph + filterDiameter do
                   for j in pw - filterDiameter .. pw + filterDiameter do
                       if i < 0 || i >= image.Height || j < 0 || j >= image.Width then
                           float32 image.Data[p]
                       else
                           float32 image.Data[i * image.Width + j] |]

        Array.fold2 (fun acc x y -> acc + x * y) 0.0f filter dataToHandle

    MyImage(Array.mapi (fun p _ -> byte (pixelProcessing p)) image.Data, image.Width, image.Height, image.Name, image.Extension)

let rotateCPU (isClockwise: bool) (image: MyImage) =

    let buffer = Array.zeroCreate (image.Width * image.Height)

    let weight = System.Convert.ToInt32 isClockwise

    for j in 0 .. image.Width - 1 do
        for i in 0 .. image.Height - 1 do

            let pw = j * weight + (image.Width - 1 - j) * (1 - weight)

            let ph = i * (1 - weight) + (image.Height - 1 - i) * weight

            buffer[ph + pw * image.Height] <- image.Data[j + i * image.Width]

    MyImage(buffer, image.Height, image.Width, image.Name, image.Extension)

let flipCPU (isVertical: bool) (image: MyImage) =

    let buffer = Array.zeroCreate (image.Height * image.Width)

    let weight = System.Convert.ToInt32 isVertical

    for j in 0 .. image.Width - 1 do
        for i in 0 .. image.Height - 1 do

            let pw = (image.Width - j - 1) * weight + j * (1 - weight)

            let ph = i * weight + (image.Height - i - 1) * (1 - weight)

            buffer[pw + ph * image.Width] <- image.Data[j + i * image.Width]

    MyImage(buffer, image.Width, image.Height, image.Name, image.Extension)

let resizeCPUBilinear (image: MyImage) (newWidth: int) (newHeight: int) =

    let scaleX = float32 image.Width / float32 newWidth
    let scaleY = float32 image.Height / float32 newHeight

    let buffer = Array.create (newWidth * newHeight) 0uy

    for newY = 0 to newHeight - 1 do

        let positionY = float32 newY * scaleY
        let y1 = int positionY
        let y2 = if y1 + 1 < image.Height then y1 + 1 else y1

        for newX = 0 to newWidth - 1 do

            let positionX = float32 newX * scaleX

            let x1 = int positionX
            let x2 = if x1 + 1 < image.Width then x1 + 1 else x1

            let weightBottom =
                if x1 = x2 then
                    float32 image.Data[y2 * image.Width + x1]
                else
                    (float32 image.Data[y2 * image.Width + x2]) * (positionX - float32 x1)
                    + (float32 image.Data[y2 * image.Width + x1]) * (float32 x2 - positionX)

            let weightTop =
                if x1 = x2 then
                    float32 image.Data[y1 * image.Width + x2]
                else
                    (float32 image.Data[y1 * image.Width + x2]) * (positionX - float32 x1)
                    + (float32 image.Data[y1 * image.Width + x1]) * (float32 x2 - positionX)

            let newWeight =
                if y1 = y2 then
                    weightBottom
                else
                    weightBottom * (positionY - float32 y1) + weightTop * (float32 y2 - positionY)

            let resizedIndex = newY * newWidth + newX

            buffer[resizedIndex] <- byte (int newWeight)

    MyImage(buffer, newWidth, newHeight, image.Name, image.Extension)

let resizeCPUNearestNeighbour (image: MyImage) (newWidth: int) (newHeight: int) =

    let scaleX = float32 image.Width / float32 newWidth
    let scaleY = float32 image.Height / float32 newHeight

    let buffer = Array.create (newWidth * newHeight) 0uy

    for newY = 0 to newHeight - 1 do
        let originalY = int (float32 newY * scaleY)

        for newX = 0 to newWidth - 1 do
            let originalX = int (float32 newX * scaleX)
            let originalIndex = originalY * image.Width + originalX
            let resizedIndex = newY * newWidth + newX
            buffer[resizedIndex] <- image.Data[originalIndex]

    MyImage(buffer, newWidth, newHeight, image.Name, image.Extension)

let cropCPU (image: MyImage) (x: int) (y: int) (newWidth: int) (newHeight: int) =

    let buffer = Array.create (newWidth * newHeight) 0uy

    for row = 0 to newHeight - 1 do
        for column = 0 to newWidth - 1 do

            let originalX = x + column
            let originalY = y + row

            buffer[row * image.Width + column] <- image.Data[originalY * newWidth + originalX]

    MyImage(buffer, newWidth, newHeight, image.Name, image.Extension)
