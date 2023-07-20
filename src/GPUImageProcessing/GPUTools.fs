module GPUTools

open Brahma.FSharp

/// <summary>
/// Applies a filter to the specified image using GPU.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes a filter kernel and an image, and asynchronously applies the filter to the image using GPU.
/// </returns>
let applyFilter (clContext: ClContext) (localWorkSize: int) =

    let applyFilterKernel = GPUKernels.applyFilter clContext localWorkSize
    let queue = clContext.QueueProvider.CreateQueue()

    fun (filter: float32[,]) (image: BasicTools.MyImage) ->

        let input =
            clContext.CreateClArray<byte>(image.Data, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

        let output =
            clContext.CreateClArray(image.Height * image.Width, HostAccessMode.NotAccessible, DeviceAccessMode.WriteOnly)

        let filterDiameter = (Array2D.length1 filter) / 2
        let filter = Helper.toFlatArray filter

        let clFilter =
            clContext.CreateClArray<float32>(filter, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

        let result = Array.zeroCreate (image.Height * image.Width)

        let result =
            queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(applyFilterKernel queue clFilter filterDiameter input image.Height image.Width output, result, ch))

        queue.Post(Msg.CreateFreeMsg clFilter)
        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)

        BasicTools.MyImage(result, image.Width, image.Height, image.Name, image.Extension)

/// <summary>
/// Rotates the image clockwise or counterclockwise using GPU.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes a boolean value indicating the rotation direction (true for clockwise, false for counterclockwise),
/// and an image, and asynchronously rotates the image using the GPU.
/// </returns>
let rotate (clContext: ClContext) (localWorkSize: int) =

    let rotateKernel = GPUKernels.rotate clContext localWorkSize
    let queue = clContext.QueueProvider.CreateQueue()

    fun (isClockwise: bool) (image: BasicTools.MyImage) ->

        let input =
            clContext.CreateClArray<byte>(image.Data, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

        let output =
            clContext.CreateClArray(image.Height * image.Width, HostAccessMode.NotAccessible, DeviceAccessMode.WriteOnly)

        let weight = System.Convert.ToInt32 isClockwise

        let result = Array.zeroCreate (image.Height * image.Width)

        let result =
            queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(rotateKernel queue weight input image.Height image.Width output, result, ch))

        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)

        BasicTools.MyImage(result, image.Height, image.Width, image.Name, image.Extension)

/// <summary>
/// Flips the image vertically or horizontally using GPU.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes a boolean value indicating the flip direction (true for vertical, false for horizontal),
/// and an image, and asynchronously flips the image using the GPU.
/// </returns>
let flip (clContext: ClContext) (localWorkSize: int) =

    let flipKernel = GPUKernels.flip clContext localWorkSize
    let queue = clContext.QueueProvider.CreateQueue()

    fun (isVertical: bool) (image: BasicTools.MyImage) ->

        let input =
            clContext.CreateClArray<byte>(image.Data, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

        let output =
            clContext.CreateClArray(image.Height * image.Width, HostAccessMode.NotAccessible, DeviceAccessMode.WriteOnly)

        let weight = System.Convert.ToInt32 isVertical

        let result = Array.zeroCreate (image.Height * image.Width)

        let result =
            queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(flipKernel queue weight input image.Height image.Width output, result, ch))

        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)

        BasicTools.MyImage(result, image.Width, image.Height, image.Name, image.Extension)

/// <summary>
/// Algorithms for resize function. The way the modified image will be formed.
/// </summary>
type ResizeAlgorithm =
    | Bilinear
    | NearestNeighbour

/// <summary>
/// Resizes the image to the specified width and height using GPU.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes the new width, new height, resize parameter and an image, and asynchronously resizes the image using the GPU.
/// </returns>
let resize (clContext: ClContext) (localWorkSize: int) =

    let resizeKernel = GPUKernels.resize clContext localWorkSize
    let queue = clContext.QueueProvider.CreateQueue()

    fun (newWidth: int) (newHeight: int) (algorithm: ResizeAlgorithm) (image: BasicTools.MyImage) ->

        let input =
            clContext.CreateClArray<byte>(image.Data, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

        let output =
            clContext.CreateClArray(newWidth * newHeight, HostAccessMode.NotAccessible, DeviceAccessMode.WriteOnly)

        let result = Array.zeroCreate (newWidth * newHeight)

        let weight =
            match algorithm with
            | Bilinear -> 1
            | NearestNeighbour -> 0

        let result =
            queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(resizeKernel queue input image.Width image.Height newWidth newHeight weight output, result, ch))

        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)

        BasicTools.MyImage(result, newWidth, newHeight, image.Name, image.Extension)

/// <summary>
/// Crops the image to a specified region using GPU.
/// </summary>
/// <param name="clContext">The representation of OpenCL context.</param>
/// <param name="localWorkSize">The size of the local work group.</param>
/// <returns>
/// A function that takes the upper-left and lower-right corner coordinates of the region to be cropped,
/// and an image, and asynchronously crops the image using the GPU.
/// </returns>
let crop (clContext: ClContext) (localWorkSize: int) =

    let cropKernel = GPUKernels.crop clContext localWorkSize
    let queue = clContext.QueueProvider.CreateQueue()

    fun (xUpper, yUpper) (xLower, yLower) (image: BasicTools.MyImage) ->

        if xUpper = xLower || yUpper = yLower then
            failwith
                $"Unequal corner points coordinates were expected, a zero-length image cannot be produced.\n
            Expected xUpper = %A{xUpper} <> xLower = %A{xLower} and yUpper = %A{yUpper} <> yLower = %A{yLower}. "

        if xLower >= image.Width || yLower >= image.Height then
            failwith
                $"Corner points coordinates are out of the image.\
            Expected xLower = %A{xLower} < image.Width = %A{image.Width} and yLower = %A{yLower} < image.Height = %A{image.Height}. "

        let input =
            clContext.CreateClArray<byte>(image.Data, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

        let newWidth = xLower - xUpper + 1
        let newHeight = yLower - yUpper + 1

        let output =
            clContext.CreateClArray(newWidth * newHeight, HostAccessMode.NotAccessible, DeviceAccessMode.WriteOnly)

        let result = Array.zeroCreate (newWidth * newHeight)

        let result =
            queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(cropKernel queue input image.Width image.Height xUpper yUpper xLower yLower output, result, ch))

        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)

        BasicTools.MyImage(result, newWidth, newHeight, image.Name, image.Extension)
