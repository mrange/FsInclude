#load "../FsInclude/FsInclude.fs"

open System
open System.IO

let IncludeFiles () =

    use s = File.OpenWrite @"IncludedFiles.fs"
    use sw = new StreamWriter(s)

    FsInclude.Processor.DownloadWebFilesToStream 
        sw 
        (Some "Included") 
        (Uri "https://raw.githubusercontent.com/fsprojects/FsLexYacc/master/src/FsLexYacc.Runtime/")
        [|
            "Lexing.fs"
            "Parsing.fs"
        |]

do IncludeFiles ()