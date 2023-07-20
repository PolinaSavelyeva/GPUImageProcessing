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

              saveAs myImage1 Png (__SOURCE_DIRECTORY__ + "/Images/output")

              let pngImage = load (__SOURCE_DIRECTORY__ + "/Images/output/1.png")

              Expect.equal pngImage.Extension ".png" $"Unexpected: %A{pngImage.Extension}.\n Expected: .png. "

          testCase "Saved real image must be of the same Gif format as the specified one"
          <| fun _ ->

              saveAs myImage1 Gif (__SOURCE_DIRECTORY__ + "/Images/output")

              let gifImage = load (__SOURCE_DIRECTORY__ + "/Images/output/1.gif")

              Expect.equal gifImage.Extension ".gif" $"Unexpected: %A{gifImage.Extension}.\n Expected: .gif. "

          testCase "Saved image must be of the same Jpeg format as the specified one"
          <| fun _ ->

              saveAs myImage1 Jpeg (__SOURCE_DIRECTORY__ + "/Images/output")

              let jpegImage = load (__SOURCE_DIRECTORY__ + "/Images/output/1.jpeg")

              Expect.equal jpegImage.Extension ".jpeg" $"Unexpected: %A{jpegImage.Extension}.\n Expected: .jpeg. "

          testCase "Saved image must be of the same Bmp format as the specified one"
          <| fun _ ->

              saveAs myImage1 Bmp (__SOURCE_DIRECTORY__ + "/Images/output")

              let bmpImage = load (__SOURCE_DIRECTORY__ + "/Images/output/1.bmp")

              Expect.equal bmpImage.Extension ".bmp" $"Unexpected: %A{bmpImage.Extension}.\n Expected: .bmp. "

          testCase "Saved image must be of the same Webp format as the specified one"
          <| fun _ ->

              saveAs myImage1 Webp (__SOURCE_DIRECTORY__ + "/Images/output")

              let webpImage = load (__SOURCE_DIRECTORY__ + "/Images/output/1.webp")

              Expect.equal webpImage.Extension ".webp" $"Unexpected: %A{webpImage.Extension}.\n Expected: .webp. "

          testCase "Saved image must be of the same Tiff format as the specified one"
          <| fun _ ->

              saveAs myImage1 Tiff (__SOURCE_DIRECTORY__ + "/Images/output")

              let tiffImage = load (__SOURCE_DIRECTORY__ + "/Images/output/1.tiff")

              Expect.equal tiffImage.Extension ".tiff" $"Unexpected: %A{tiffImage.Extension}.\n Expected: .tiff. "

          testCase "Saved image must be of the same Tga format as the specified one"
          <| fun _ ->

              saveAs myImage1 Tga (__SOURCE_DIRECTORY__ + "/Images/output")

              let tgaImage = load (__SOURCE_DIRECTORY__ + "/Images/output/1.tga")

              Expect.equal tgaImage.Extension ".tga" $"Unexpected: %A{tgaImage.Extension}.\n Expected: .tga. "

          testCase "Saved image must be of the same Pbm format as the specified one"
          <| fun _ ->

              saveAs myImage1 Pbm (__SOURCE_DIRECTORY__ + "/Images/output")

              let pbmImage = load (__SOURCE_DIRECTORY__ + "/Images/output/1.pbm")

              Expect.equal pbmImage.Extension ".pbm" $"Unexpected: %A{pbmImage.Extension}.\n Expected: .pbm. "

          testPropertyWithConfig myConfig "Saved generated image must be of the format as the specified one"
          <| fun myImage (format: ImageFormats) ->

              saveAs myImage format (__SOURCE_DIRECTORY__ + "/Images/output")

              let parsedFormat = imageFormatsParser format

              let image =
                  load (__SOURCE_DIRECTORY__ + "/Images/output/" + myImage.Name + parsedFormat)

              Expect.equal image.Extension parsedFormat $"Unexpected: %A{image.Extension}.\n Expected: %A{parsedFormat}. " ]
