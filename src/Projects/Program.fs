// Copyright 2015 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

open FsInclude

open System
open System.IO

open Disposable

[<EntryPoint>]
let main argv =

    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

    Log.infof "Testing %d" 1

    use xx = onExitDo <| fun () -> printfn "OnExit"

    printfn "Hello"


(*
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
*)

    use s = File.OpenWrite @"..\..\File.fs"
    use sw = new StreamWriter(s)

    FsInclude.Processor.DownloadWebFilesToStream
        sw
        (Some "Included")
        (Uri "https://raw.githubusercontent.com/mrange/FsInclude/master/src/")
        [|
            "Common/Disposable.fs"
            "Common/Disposable.fs"
            "Common/XXX.fs"
        |]

    0
