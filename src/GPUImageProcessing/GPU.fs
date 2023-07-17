module GPU

open Brahma.FSharp

let applyFilterGPUKernel (clContext: ClContext) localWorkSize =

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

let rotateGPUKernel (clContext: ClContext) localWorkSize =

    let kernel =
        <@
            fun (range: Range1D) (image: ClArray<byte>) imageWidth imageHeight (weight: ClCell<int>) (result: ClArray<byte>) ->
                let p = range.GlobalID0
                let i = p / imageWidth
                let j = p % imageWidth
                let weight = weight.Value

                if i < imageHeight then
                    let pw = j * weight + (imageWidth - 1 - j) * (1 - weight)
                    let ph = i * (1 - weight) + (imageHeight - 1 - i) * weight
                    result[ph + pw * imageHeight] <- image[p]
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<Msg>) (weight: ClCell<int>) (image: ClArray<byte>) imageHeight imageWidth (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(imageHeight * imageWidth, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange image imageWidth imageHeight weight result))
        commandQueue.Post(Msg.CreateRunMsg<INDRange, obj> kernel)
        result

let flipGPUKernel (clContext: ClContext) localWorkSize =

    let kernel =
        <@
            fun (range: Range1D) (image: ClArray<byte>) imageWidth imageHeight (weight: ClCell<int>) (result: ClArray<byte>) ->
                let p = range.GlobalID0
                let i = p / imageWidth
                let j = p % imageWidth
                let weight = weight.Value

                if i < imageHeight then
                    let pw = (imageWidth - j - 1) * weight + j * (1 - weight)
                    let ph = i * weight + (imageHeight - i - 1) * (1 - weight)
                    result[pw + ph * imageWidth] <- image[p]
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<Msg>) (weight: ClCell<int>) (image: ClArray<byte>) imageHeight imageWidth (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(imageHeight * imageWidth, localWorkSize)
        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange image imageWidth imageHeight weight result))
        commandQueue.Post(Msg.CreateRunMsg<INDRange, obj> kernel)
        result

let applyFilter (clContext: ClContext) (localWorkSize: int) =

    let applyFilterKernel = applyFilterGPUKernel clContext localWorkSize
    let queue = clContext.QueueProvider.CreateQueue()

    fun (filter: float32[,]) (image: MyImage.MyImage) ->

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

        MyImage.MyImage(result, image.Width, image.Height, image.Name)

let rotate (clContext: ClContext) (localWorkSize: int) =

    let rotateKernel = rotateGPUKernel clContext localWorkSize
    let queue = clContext.QueueProvider.CreateQueue()

    fun (isClockwise: bool) (image: MyImage.MyImage) ->

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

        MyImage.MyImage(result, image.Height, image.Width, image.Name)

let flip (clContext: ClContext) (localWorkSize: int) =

    let flipKernel = flipGPUKernel clContext localWorkSize
    let queue = clContext.QueueProvider.CreateQueue()

    fun (isVertical: bool) (image: MyImage.MyImage) ->

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

        MyImage.MyImage(result, image.Width, image.Height, image.Name)
