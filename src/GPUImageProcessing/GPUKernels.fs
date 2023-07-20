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
            fun (range: Range1D) (image: ClArray<byte>) imageWidth imageHeight newWidth newHeight weight (result: ClArray<byte>) ->
                let p = range.GlobalID0
                let py = p / newWidth
                let px = p % newWidth
                let scaleX = float32 imageWidth / float32 newWidth
                let scaleY = float32 imageHeight / float32 newHeight

                if py < newHeight then
                    let scaledX = float32 px * scaleX
                    let scaledY = float32 py * scaleY
                    let xLower = px * imageWidth / newWidth
                    let yLower = py * imageHeight / newHeight

                    if weight = 1 then
                        let xUpper = if xLower + 1 < imageWidth then xLower + 1 else xLower
                        let yUpper = if yLower + 1 < imageHeight then yLower + 1 else yLower

                        let weightBottom =
                            if xLower = xUpper then
                                float32 image[yUpper * imageWidth + xLower]
                            else
                                (float32 image[yUpper * imageWidth + xUpper]) * (scaledX - float32 xLower)
                                + (float32 image[yUpper * imageWidth + xLower]) * (float32 xUpper - scaledX)

                        let weightTop =
                            if xLower = xUpper then
                                float32 image[yLower * imageWidth + xUpper]
                            else
                                (float32 image[yLower * imageWidth + xUpper]) * (scaledX - float32 xLower)
                                + (float32 image[yLower * imageWidth + xLower]) * (float32 xUpper - scaledX)

                        let newWeight =
                            if yLower = yUpper then
                                weightBottom
                            else
                                weightBottom * (scaledY - float32 yLower) + weightTop * (float32 yUpper - scaledY)

                        if
                            p < newWidth * newHeight
                            && xLower < imageWidth
                            && yLower < imageHeight
                            && xUpper < imageWidth
                            && yUpper < imageHeight
                        then
                            result[p] <- byte (int newWeight)
                    elif xLower < imageWidth && yLower < imageHeight then
                        let originalIndex = yLower * imageWidth + xLower

                        if p < newWidth * newHeight && originalIndex < imageWidth * imageHeight then
                            result[p] <- image[originalIndex]
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<Msg>) (image: ClArray<byte>) imageWidth imageHeight newWidth newHeight weight (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(newWidth * newHeight, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange image imageWidth imageHeight newWidth newHeight weight result))
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
            fun (range: Range1D) (watermark: ClArray<byte>) imageWidth imageHeight imageCenterX imageCenterY watermarkCenterX watermarkCenterY watermarkWidth (result: ClArray<byte>) ->
                let p = range.GlobalID0
                let py = p / watermarkWidth
                let px = p % watermarkWidth
                let centerDistanceY = py - watermarkCenterY
                let centerDistanceX = px - watermarkCenterX

                let index =
                    (imageCenterY + centerDistanceY) * imageWidth + (imageCenterX + centerDistanceX)

                if py < imageHeight && index < imageHeight * imageWidth && index >= 0 then
                    result[index] <- watermark[p]
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<Msg>) (watermark: ClArray<byte>) imageWidth imageHeight imageCenterX imageCenterY watermarkCenterX watermarkCenterY watermarkWidth watermarkHeight (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(watermarkWidth * watermarkHeight, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange watermark imageWidth imageHeight imageCenterX imageCenterY watermarkCenterX watermarkCenterY watermarkWidth result))
        commandQueue.Post(Msg.CreateRunMsg<INDRange, obj> kernel)

        result
