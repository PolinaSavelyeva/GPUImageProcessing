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
