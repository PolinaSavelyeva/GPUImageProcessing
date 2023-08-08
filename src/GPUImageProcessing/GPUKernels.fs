module GPUKernels

open Brahma.FSharp


/// <summary>
/// Creates compiled GPU filter kernel.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes a command queue, filter parameters, image data, and result array as inputs
/// and asynchronously applies the filter kernel to the image using the GPU.
/// The resulting image data is stored in the result array.
/// </returns>
let applyFilter (clContext: ClContext) localWorkSize =

    let kernel =
        <@
            fun (range: Range1D) (image: ClArray<byte>) imageWidth imageHeight (filter: ClArray<float32>) filterDiameter (result: ClArray<byte>) ->
                let p = range.GlobalID0
                let py = p % imageWidth
                let px = p / imageWidth
                let mutable res = 0.0f

                for i in px - filterDiameter .. px + filterDiameter do
                    for j in py - filterDiameter .. py + filterDiameter do
                        let f =
                            filter[(i - px + filterDiameter) * (2 * filterDiameter + 1) + (j - py + filterDiameter)]

                        if i < 0 || i >= imageHeight || j < 0 || j >= imageWidth then
                            res <- res + (float32 image[p]) * f
                        else
                            res <- res + (float32 image[i * imageWidth + j]) * f

                result[p] <- byte (int res)
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<Msg>) (filter: ClArray<float32>) filterDiameter (image: ClArray<byte>) imageHeight imageWidth (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(imageHeight * imageWidth, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange image imageWidth imageHeight filter filterDiameter result))
        commandQueue.Post(Msg.CreateRunMsg<INDRange, obj> kernel)

        result

/// <summary>
/// Creates compiled GPU rotation kernel.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes a command queue, rotation parameter, image data, and result array as inputs
/// and asynchronously rotates the image using the GPU.
/// The resulting image data is stored in the result array.
/// </returns>
let rotate (clContext: ClContext) localWorkSize =

    let kernel =
        <@
            fun (range: Range1D) (image: ClArray<byte>) imageWidth imageHeight weight (result: ClArray<byte>) ->
                let p = range.GlobalID0
                let py = p / imageWidth
                let px = p % imageWidth

                if py < imageHeight then
                    let pw = px * weight + (imageWidth - 1 - px) * (1 - weight)
                    let ph = py * (1 - weight) + (imageHeight - 1 - py) * weight
                    result[ph + pw * imageHeight] <- image[p]
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<Msg>) weight (image: ClArray<byte>) imageHeight imageWidth (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(imageHeight * imageWidth, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange image imageWidth imageHeight weight result))
        commandQueue.Post(Msg.CreateRunMsg<INDRange, obj> kernel)

        result

/// <summary>
/// Creates compiled GPU flip kernel.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes a command queue, flip parameter, image data, and result array as inputs
/// and asynchronously flips the image using the GPU.
/// The resulting image data is stored in the result array.
/// </returns>
let flip (clContext: ClContext) localWorkSize =

    let kernel =
        <@
            fun (range: Range1D) (image: ClArray<byte>) imageWidth imageHeight weight (result: ClArray<byte>) ->
                let p = range.GlobalID0
                let py = p / imageWidth
                let px = p % imageWidth

                if py < imageHeight then
                    let pw = (imageWidth - px - 1) * weight + px * (1 - weight)
                    let ph = py * weight + (imageHeight - py - 1) * (1 - weight)
                    result[pw + ph * imageWidth] <- image[p]
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<Msg>) weight (image: ClArray<byte>) imageHeight imageWidth (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(imageHeight * imageWidth, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange image imageWidth imageHeight weight result))
        commandQueue.Post(Msg.CreateRunMsg<INDRange, obj> kernel)

        result

/// <summary>
/// Creates compiled GPU resize kernel.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes an image, its original width and height, new width and new height, resize algorithm parameter
/// and asynchronously resizes the image using the GPU.
/// The resulting image data is stored in the result array.
/// </returns>
let resize (clContext: ClContext) localWorkSize =

    let kernel =
        <@
            fun (range: Range1D) (image: ClArray<byte>) imageWidth imageHeight newWidth newHeight (scaleX: ClCell<float32>) (scaleY: ClCell<float32>) weight (result: ClArray<byte>) ->
                let p = range.GlobalID0
                let py = p / newWidth
                let px = p % newWidth
                let positionY = float32 py * scaleY.Value
                let positionX = float32 px * scaleX.Value
                let xLower = int positionX
                let yLower = int positionY

                if py < newHeight && xLower < imageWidth && yLower < imageHeight && xLower >= 0 && yLower >= 0 then
                    if weight = 1 then
                        let yUpper = if yLower + 1 < imageHeight then yLower + 1 else yLower
                        let xUpper = if xLower + 1 < imageWidth then xLower + 1 else xLower

                        if
                            xUpper < imageWidth
                            && yUpper < imageHeight
                            && xUpper >= 0
                            && yUpper >= 0
                            && xLower <= xUpper
                            && yLower <= yUpper
                        then
                            let weightBottom =
                                if xLower = xUpper then
                                    float32 image[yUpper * imageWidth + xLower]
                                else
                                    let fDataXUpper = float32 image[yUpper * imageWidth + xUpper]
                                    let fDataXLower = float32 image[yUpper * imageWidth + xLower]
                                    let fXLower = float32 xLower
                                    let fXUpper = float32 xUpper
                                    let positionLower = positionX - fXLower
                                    let positionUpper = fXUpper - positionX
                                    let one = fDataXUpper * positionLower
                                    let two = fDataXLower * positionUpper
                                    one + two

                            let weightTop =
                                if xLower = xUpper then
                                    float32 image[yLower * imageWidth + xUpper]
                                else
                                    let fDataXUpper = float32 image[yLower * imageWidth + xUpper]
                                    let fDataXLower = float32 image[yLower * imageWidth + xLower]
                                    let fXUpper = float32 xUpper
                                    let fXLower = float32 xLower
                                    let positionLower = positionX - fXLower
                                    let positionUpper = fXUpper - positionX
                                    let one = fDataXUpper * positionLower
                                    let two = fDataXLower * positionUpper
                                    one + two

                            let newWeight =
                                if yLower = yUpper then
                                    weightBottom
                                else
                                    let fYUpper = float32 yUpper
                                    let fYLower = float32 yLower
                                    let distYUpper = fYUpper - positionY
                                    let distYLower = positionY - fYLower
                                    let one = weightTop * distYUpper
                                    let two = weightBottom * distYLower
                                    one + two

                            result[p] <- byte (int newWeight)
                    else
                        let originalIndex = yLower * imageWidth + xLower

                        if originalIndex < imageWidth * imageHeight then
                            result[p] <- image[originalIndex]
        @>

    let kernel = clContext.Compile kernel


    fun (commandQueue: MailboxProcessor<Msg>) (image: ClArray<byte>) imageWidth imageHeight newWidth newHeight scaleX scaleY weight (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(newWidth * newHeight, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange image imageWidth imageHeight newWidth newHeight scaleX scaleY weight result))
        commandQueue.Post(Msg.CreateRunMsg<INDRange, obj> kernel)

        result

/// <summary>
/// Creates compiled GPU crop kernel.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes image, original width and height of it,
/// coordinates of the upper-left corner and the lower-right corner of the region to be cropped,
/// and asynchronously crops the image using GPU.
/// The resulting image data is stored in the result array.
/// </returns>
let crop (clContext: ClContext) localWorkSize =

    let kernel =
        <@
            fun (range: Range1D) (image: ClArray<byte>) imageWidth newWidth newHeight xUpper yUpper (result: ClArray<byte>) ->
                let p = range.GlobalID0
                let py = p / newWidth
                let px = p % newWidth

                if py < newHeight then
                    result[p] <- image[(py + yUpper) * imageWidth + (xUpper + px)]
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<Msg>) (image: ClArray<byte>) imageWidth newWidth newHeight xUpper yUpper (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(newWidth * newHeight, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange image imageWidth newWidth newHeight xUpper yUpper result))
        commandQueue.Post(Msg.CreateRunMsg<INDRange, obj> kernel)

        result

/// <summary>
/// Creates compiled GPU watermark kernel.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes the watermark data as image, the original width and height of the image,
/// the coordinates of the center of the image,
/// the coordinates of the center of the watermark,
/// the width and height of the watermark,
/// and asynchronously applies the watermark to the image using the GPU.
/// The resulting image data is stored in the result array.
/// </returns>
let watermark (clContext: ClContext) localWorkSize =

    let kernel =
        <@
            fun (range: Range1D) (watermark: ClArray<byte>) imageWidth imageHeight imageCenterX imageCenterY watermarkCenterX watermarkCenterY watermarkWidth watermarkHeight (result: ClArray<byte>) ->
                let p = range.GlobalID0
                let py = p / watermarkWidth
                let px = p % watermarkWidth

                if py < watermarkHeight then
                    let centerDistanceY = py - watermarkCenterY
                    let centerDistanceX = px - watermarkCenterX
                    let originalImageY = imageCenterY + centerDistanceY
                    let originalImageX = imageCenterX + centerDistanceX

                    if
                        originalImageY < imageHeight
                        && originalImageY >= 0
                        && originalImageX < imageWidth
                        && originalImageX >= 0
                    then
                        let index = (originalImageY * imageWidth) + originalImageX
                        result[index] <- watermark[p]
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<Msg>) (watermark: ClArray<byte>) imageWidth imageHeight imageCenterX imageCenterY watermarkCenterX watermarkCenterY watermarkWidth watermarkHeight (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(watermark.Length, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(
            Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange watermark imageWidth imageHeight imageCenterX imageCenterY watermarkCenterX watermarkCenterY watermarkWidth watermarkHeight result)
        )

        commandQueue.Post(Msg.CreateRunMsg<INDRange, obj> kernel)

        result
