module MyImage

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

type MyImage =
    val Data: array<byte>
    val Width: int
    val Height: int
    val Name: string

    new(data, width, height, name) =
        { Data = data
          Width = width
          Height = height
          Name = name }

let load (filePath: string) =

    let image = Image.Load<L8> filePath
    let buffer = Array.zeroCreate<byte> (image.Width * image.Height)
    image.CopyPixelDataTo(System.Span<byte> buffer)

    MyImage(buffer, image.Width, image.Height, System.IO.Path.GetFileName filePath)

type ImageFormats =
    | Png
    | Gif
    | Jpeg
    | Bmp
    | Webp
    | Tiff
    | Tga
    | Pbm

let save (image: MyImage) (savingFormat : ImageFormats) (filePath : string) =

    let image =
        Image.LoadPixelData<L8>(image.Data, image.Width, image.Height)

    match savingFormat with
    | Png -> image.SaveAsPng filePath
    | Gif -> image.SaveAsGif filePath
    | Jpeg -> image.SaveAsJpeg filePath
    | Bmp -> image.SaveAsBmp filePath
    | Webp -> image.SaveAsWebp filePath
    | Tiff -> image.SaveAsTiff filePath
    | Tga -> image.SaveAsTga filePath
    | Pbm -> image.SaveAsPbm filePath
