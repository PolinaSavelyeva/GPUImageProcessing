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
                let pw = p % imageWidth
                let ph = p / imageWidth
                let mutable res = 0.0f

                for i in ph - filterDiameter .. ph + filterDiameter do
                    for j in pw - filterDiameter .. pw + filterDiameter do
                        let f =
                            filter[(i - ph + filterDiameter) * (2 * filterDiameter + 1) + (j - pw + filterDiameter)]

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
                let i = p / imageWidth
                let j = p % imageWidth

                if i < imageHeight then
                    let pw = j * weight + (imageWidth - 1 - j) * (1 - weight)
                    let ph = i * (1 - weight) + (imageHeight - 1 - i) * weight
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
                let i = p / imageWidth
                let j = p % imageWidth

                if i < imageHeight then
                    let pw = (imageWidth - j - 1) * weight + j * (1 - weight)
                    let ph = i * weight + (imageHeight - i - 1) * (1 - weight)
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
                let scaleX = float32 imageWidth / float32 newWidth
                let scaleY = float32 imageHeight / float32 newHeight
                let p = range.GlobalID0
                let newY = p / newWidth
                let newX = p % newWidth

                if newY < newHeight then
                    let positionX = float32 newX * scaleX
                    let positionY = float32 newY * scaleY
                    let x1 = newX * imageWidth / newWidth
                    let y1 = newY * imageHeight / newHeight

                    if weight = 1 then
                        let x2 = if x1 + 1 < imageWidth then x1 + 1 else x1
                        let y2 = if y1 + 1 < imageHeight then y1 + 1 else y1

                        let weightBottom =
                            if x1 = x2 then
                                float32 image[y2 * imageWidth + x1]
                            else
                                (float32 image[y2 * imageWidth + x2]) * (positionX - float32 x1)
                                + (float32 image[y2 * imageWidth + x1]) * (float32 x2 - positionX)

                        let weightTop =
                            if x1 = x2 then
                                float32 image[y1 * imageWidth + x2]
                            else
                                (float32 image[y1 * imageWidth + x2]) * (positionX - float32 x1)
                                + (float32 image[y1 * imageWidth + x1]) * (float32 x2 - positionX)

                        let newWeight =
                            if y1 = y2 then
                                weightBottom
                            else
                                weightBottom * (positionY - float32 y1) + weightTop * (float32 y2 - positionY)

                        if
                            p < newWidth * newHeight
                            && x1 < imageWidth
                            && y1 < imageHeight
                            && x2 < imageWidth
                            && y2 < imageHeight
                        then
                            result[p] <- byte (int newWeight)
                    elif x1 < imageWidth && y1 < imageHeight then
                        let originalIndex = y1 * imageWidth + x1

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
                let i = p / newWidth
                let j = p % newWidth

                if i < newHeight then
                    result[p] <- image[(i + yUpper) * imageWidth + (xUpper + j)]
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<Msg>) (image: ClArray<byte>) imageWidth imageHeight xUpper yUpper xLower yLower (result: ClArray<byte>) ->

        let newWidth = xLower - xUpper + 1
        let newHeight = yLower - yUpper + 1

        let ndRange = Range1D.CreateValid(newWidth * newHeight, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange image imageWidth newWidth newHeight xUpper yUpper result))
        commandQueue.Post(Msg.CreateRunMsg<INDRange, obj> kernel)

        result
