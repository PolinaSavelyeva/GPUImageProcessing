namespace Tests

module ExpectoTemplate =

    open Expecto
    open Helper


    [<EntryPoint>]
    let main argv =
        System.IO.Path.Combine(src, "Images", "output") |> deleteFilesInDirectory

        runTestsInAssembly defaultConfig argv
