open System
open System.IO

[<EntryPoint>]
let main argv = 

    use s = File.OpenWrite @"..\..\File.fs"
    use sw = new StreamWriter(s)

    FsInclude.Processor.DownloadWebFilesToStream 
        sw 
        (Some "Included") 
        (Uri "https://raw.githubusercontent.com/fsprojects/FsLexYacc/master/src/FsLexYacc.Runtime/")
        [|
            "Lexing.fs"
            "Parsing.fs"
        |]

    0
