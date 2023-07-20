module SaveAs

open BasicTools
open Helper
open Expecto

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "Saved real image must be of the same Png format as the specified one"
          <| fun _ ->

              saveAs myImage1 Png <| System.IO.Path.Combine(src, "Images", "output")

              let pngImage = load <| System.IO.Path.Combine(src, "Images", "output", "1.png")

              Expect.equal pngImage.Extension ".png" $"Unexpected: %A{pngImage.Extension}.\n Expected: .png. "

          testCase "Saved real image must be of the same Gif format as the specified one"
          <| fun _ ->

              saveAs myImage1 Gif <| System.IO.Path.Combine(src, "Images", "output")

              let gifImage = load <| System.IO.Path.Combine(src, "Images", "output", "1.gif")

              Expect.equal gifImage.Extension ".gif" $"Unexpected: %A{gifImage.Extension}.\n Expected: .gif. "

          testCase "Saved image must be of the same Jpeg format as the specified one"
          <| fun _ ->

              saveAs myImage1 Jpeg <| System.IO.Path.Combine(src, "Images", "output")

              let jpegImage = load <| System.IO.Path.Combine(src, "Images", "output", "1.jpeg")

              Expect.equal jpegImage.Extension ".jpeg" $"Unexpected: %A{jpegImage.Extension}.\n Expected: .jpeg. "

          testCase "Saved image must be of the same Bmp format as the specified one"
          <| fun _ ->

              saveAs myImage1 Bmp <| System.IO.Path.Combine(src, "Images", "output")

              let bmpImage = load <| System.IO.Path.Combine(src, "Images", "output", "1.bmp")

              Expect.equal bmpImage.Extension ".bmp" $"Unexpected: %A{bmpImage.Extension}.\n Expected: .bmp. "

          testCase "Saved image must be of the same Webp format as the specified one"
          <| fun _ ->

              saveAs myImage1 Webp <| System.IO.Path.Combine(src, "Images", "output")

              let webpImage = load <| System.IO.Path.Combine(src, "Images", "output", "1.webp")

              Expect.equal webpImage.Extension ".webp" $"Unexpected: %A{webpImage.Extension}.\n Expected: .webp. "

          testCase "Saved image must be of the same Tiff format as the specified one"
          <| fun _ ->

              saveAs myImage1 Tiff <| System.IO.Path.Combine(src, "Images", "output")

              let tiffImage = load <| System.IO.Path.Combine(src, "Images", "output", "1.tiff")

              Expect.equal tiffImage.Extension ".tiff" $"Unexpected: %A{tiffImage.Extension}.\n Expected: .tiff. "

          testCase "Saved image must be of the same Tga format as the specified one"
          <| fun _ ->

              saveAs myImage1 Tga <| System.IO.Path.Combine(src, "Images", "output")

              let tgaImage = load <| System.IO.Path.Combine(src, "Images", "output", "1.tga")

              Expect.equal tgaImage.Extension ".tga" $"Unexpected: %A{tgaImage.Extension}.\n Expected: .tga. "

          testCase "Saved image must be of the same Pbm format as the specified one"
          <| fun _ ->

              saveAs myImage1 Pbm <| System.IO.Path.Combine(src, "Images", "output")

              let pbmImage = load <| System.IO.Path.Combine(src, "Images", "output", "1.pbm")

              Expect.equal pbmImage.Extension ".pbm" $"Unexpected: %A{pbmImage.Extension}.\n Expected: .pbm. "

          testPropertyWithConfig myConfig "Saved generated image must be of the format as the specified one"
          <| fun myImage (format: ImageFormats) ->

              saveAs myImage format <| System.IO.Path.Combine(src, "Images", "output")

              let parsedFormat = imageFormatsParser format

              let image =
                  load <| System.IO.Path.Combine(src, "Images", "output", myImage.Name + parsedFormat)

              Expect.equal image.Extension parsedFormat $"Unexpected: %A{image.Extension}.\n Expected: %A{parsedFormat}. " ]
