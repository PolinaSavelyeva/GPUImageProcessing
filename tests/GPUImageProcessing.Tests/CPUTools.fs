module CPUTools

open BasicTools

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

let resizeCPUBilinear (newWidth: int) (newHeight: int) (image: MyImage) =

    if newWidth <= 0 || newHeight <= 0 then
        failwith $"Expected positive new sides, but given newWidth = %A{newWidth} and newHeight = %A{newHeight}. "

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

let resizeCPUNearestNeighbour (newWidth: int) (newHeight: int) (image: MyImage) =

    if newWidth <= 0 || newHeight <= 0 then
        failwith $"Expected positive new sides, but given newWidth = %A{newWidth} and newHeight = %A{newHeight}. "

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

let cropCPU (xUpper, yUpper) (xLower, yLower) (image: MyImage) =

    if xUpper = xLower || yUpper = yLower then
        failwith
            $"Unequal corner points coordinates were expected, a zero-length image cannot be produced.\n
                   Expected xUpper = %A{xUpper} <> xLower = %A{xLower} and yUpper = %A{yUpper} <> yLower = %A{yLower}. "

    if xLower >= image.Width || yLower >= image.Height then
        failwith
            $"Corner points coordinates are out of the image.\n
                  Expected xLower = %A{xLower} < image.Width = %A{image.Width} and yLower = %A{yLower} < image.Height = %A{image.Height}. "

    let newWidth = xLower - xUpper + 1
    let newHeight = yLower - yUpper + 1

    let buffer = Array.create (newWidth * newHeight) 0uy

    for y = 0 to newHeight - 1 do
        for x = 0 to newWidth - 1 do

            buffer[y * newWidth + x] <- image.Data[(y + yUpper) * image.Width + (x + xUpper)]

    MyImage(buffer, newWidth, newHeight, image.Name, image.Extension)

let watermarkCPU (watermark: MyImage) (watermarkScale: float) (image: MyImage) =

    if watermarkScale <= 0 then
        failwith $"Expected positive watermark scale, but given %A{watermarkScale}. "

    let imageCenterX = image.Width / 2
    let imageCenterY = image.Height / 2

    let watermarkWidth = int (float watermark.Width / watermarkScale)
    let watermarkHeight = int (float watermark.Height / watermarkScale)

    let watermarkCenterX = watermarkWidth / 2
    let watermarkCenterY = watermarkHeight / 2

    let resizedWatermark =
        resizeCPUNearestNeighbour watermarkWidth watermarkHeight watermark

    let buffer = image.Data

    for y in 0 .. watermarkHeight - 1 do
        let distanceY = y - watermarkCenterY

        for x in 0 .. watermarkWidth - 1 do

            let distanceX = x - watermarkCenterX
            let index = (imageCenterY + distanceY) * image.Width + (imageCenterX + distanceX)

            if index < image.Height * image.Width && index >= 0 then
                buffer[index] <- resizedWatermark.Data[y * watermarkWidth + x]

    MyImage(buffer, image.Width, image.Height, image.Name, image.Extension)
