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
            clContext.CreateClArray(image.Height * image.Width, HostAccessMode.NotAccessible, DeviceAccessMode.WriteOnly, allocationMode = AllocationMode.Default)

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

        BasicTools.MyImage(result, image.Width, image.Height, image.Name)

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
            clContext.CreateClArray(image.Height * image.Width, HostAccessMode.NotAccessible, DeviceAccessMode.WriteOnly, allocationMode = AllocationMode.Default)

        let weight = System.Convert.ToInt32 isClockwise
        let clWeight = clContext.CreateClCell(weight)

        let result = Array.zeroCreate (image.Height * image.Width)

        let result =
            queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(rotateKernel queue clWeight input image.Height image.Width output, result, ch))

        queue.Post(Msg.CreateFreeMsg clWeight)
        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)

        BasicTools.MyImage(result, image.Height, image.Width, image.Name)

// <summary>
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
            clContext.CreateClArray(image.Height * image.Width, HostAccessMode.NotAccessible, DeviceAccessMode.WriteOnly, allocationMode = AllocationMode.Default)

        let weight = System.Convert.ToInt32 isVertical
        let clWeight = clContext.CreateClCell(weight)

        let result = Array.zeroCreate (image.Height * image.Width)

        let result =
            queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(flipKernel queue clWeight input image.Height image.Width output, result, ch))

        queue.Post(Msg.CreateFreeMsg clWeight)
        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)

        BasicTools.MyImage(result, image.Width, image.Height, image.Name)
