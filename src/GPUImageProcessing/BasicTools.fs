module BasicTools

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

/// <summary>
/// Encapsulates an image, which includes both the byte pixel data and its associated attributes.
/// </summary>
type MyImage =
    val Data: array<byte>
    val Width: int
    val Height: int
    val mutable Name: string
    val Extension: string

    new(data, width, height, name, extension) =
        { Data = data
          Width = width
          Height = height
          Name = name
          Extension = extension }

    new(data, width, height, nameWithExtension: string) =

        let name = System.IO.Path.GetFileNameWithoutExtension nameWithExtension
        let extension = System.IO.Path.GetExtension nameWithExtension

        MyImage(data, width, height, name, extension)

    member this.ChangeName(newName: string) = this.Name <- newName

/// <summary>
/// Loads the image located at the specified file path.
/// </summary>
/// <param name="filePath">The path where the image is located. The image name and extension are required.</param>
let load (filePath: string) =

    let image = Image.Load<L8> filePath
    let buffer = Array.zeroCreate<byte> (image.Width * image.Height)
    image.CopyPixelDataTo(System.Span<byte> buffer)

    MyImage(buffer, image.Width, image.Height, System.IO.Path.GetFileName filePath)

/// <summary>
/// Saves the image to the specified directory in the same extension as the input.
/// </summary>
/// <param name="image">Saved image.</param>
/// <param name="directoryPath">Path to the directory where the image will be saved.</param>
let save (image: MyImage) directoryPath =

    let filePath = directoryPath + "/" + image.Name + image.Extension
    let image = Image.LoadPixelData<L8>(image.Data, image.Width, image.Height)

    image.Save filePath

/// <summary>
/// Represents the supported image formats for saving images.
/// </summary>
type ImageFormats =
    | Png
    | Gif
    | Jpeg
    | Bmp
    | Webp
    | Tiff
    | Tga
    | Pbm

let imageFormatsParser imageFormat =
    match imageFormat with
    | Png -> ".png"
    | Gif -> ".gif"
    | Jpeg -> ".jpeg"
    | Bmp -> ".bmp"
    | Webp -> ".webp"
    | Tiff -> ".tiff"
    | Tga -> ".tga"
    | Pbm -> ".pbm"

/// <summary>
/// Saves the image in the selected format to the specified directory.
/// </summary>
/// <param name="image">Saved image.</param>
/// <param name="savingFormat">The format in which the image will be saved.</param>
/// <param name="directoryPath">Path to the directory where the image will be saved.</param>
let saveAs (image: MyImage) savingFormat directoryPath =
    save
    <| MyImage(image.Data, image.Width, image.Height, image.Name, imageFormatsParser savingFormat)
    <| directoryPath
