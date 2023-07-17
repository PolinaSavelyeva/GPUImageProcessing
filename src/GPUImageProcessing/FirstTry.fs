module FirstTry

open Helper
open BasicTools
open Brahma.FSharp

let resize (image: MyImage) (newWidth: int) (newHeight: int) =
    let scaleX = float image.Width / float newWidth
    let scaleY = float image.Height / float newHeight

    let resizedPixels = Array.create (newWidth * newHeight) 0uy

    for newY = 0 to newHeight - 1 do
        let originalY = int (float newY * scaleY)

        for newX = 0 to newWidth - 1 do
            let originalX = int (float newX * scaleX)
            let originalIndex = originalY * image.Width + originalX
            let resizedIndex = newY * newWidth + newX
            resizedPixels[resizedIndex] <- image.Data[originalIndex]

    MyImage(resizedPixels, newWidth, newHeight, image.Name, image.Extension)
