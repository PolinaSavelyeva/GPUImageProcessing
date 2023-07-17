module Agents

open MyImage

let logger =

    MailboxProcessor.Start(fun inbox ->
        async {
            while true do
                let! message = inbox.Receive()
                printfn $"{message}"
        })

type imageMessage =
    | Image of MyImage
    | EOS of AsyncReplyChannel<unit>

let imageSaver outputDirectory =

    let initial (inbox: MailboxProcessor<imageMessage>) =

        async {
            while true do
                let! message = inbox.Receive()

                match message with
                | EOS channel ->
                    logger.Post "Image saver is finished! "
                    channel.Reply()

                | Image image ->
                    save image (Helper.generatePath outputDirectory image.Name)
                    logger.Post $"Saved: %A{image.Name}! "
        }

    MailboxProcessor.Start initial

let imageProcessor imageEditor (receiver: MailboxProcessor<_>) =

    let initial (inbox: MailboxProcessor<imageMessage>) =

        async {
            while true do
                let! message = inbox.Receive()

                match message with
                | EOS channel ->
                    logger.Post "Image processor is finished! "
                    receiver.PostAndReply EOS
                    channel.Reply()
                | Image image ->
                    let filtered = imageEditor image
                    logger.Post $"Edited: %A{image.Name}! "
                    receiver.Post(Image filtered)
        }

    MailboxProcessor.Start initial

type pathMessage =
    | Path of string
    | EOS of AsyncReplyChannel<unit>

let imageFullProcessor imageEditor outputDirectory =

    let initial (inbox: MailboxProcessor<pathMessage>) =

        async {
            while true do
                let! message = inbox.Receive()

                match message with
                | EOS channel ->
                    logger.Post "Image processor and saver is finished! "
                    channel.Reply()

                | Path path ->
                    let image = load path
                    logger.Post $"Opened: %A{image.Name}! "
                    let filtered = imageEditor image
                    logger.Post $"Edited: %A{image.Name}! "
                    save filtered (Helper.generatePath outputDirectory image.Name)
                    logger.Post $"Saved: %A{image.Name}! "

        }

    MailboxProcessor.Start initial
