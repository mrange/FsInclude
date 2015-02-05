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

#load "../packages/FsInclude.0.0.1/FsInclude/FsInclude.fs"

open System
open System.IO
open System.Text

let IncludeFiles () =

    use s = File.OpenWrite @"IncludedFiles.fs"
    use sw = new StreamWriter(s, Encoding.UTF8)

    FsInclude.Processor.DownloadWebFilesToStream
        sw
        (Some "Included")
        (Uri "https://raw.githubusercontent.com/mrange/FsInclude/master/src/")
        [|
            "Modules/BasicModule.fs"
            "Modules/StreamModule.fs"
        |]

do IncludeFiles ()
<#
    // Whenever this file is saved the files in the Includes section is downloaded
    // from GitHub (you can download from other websources by changing rootpath)
    RootPath    = @"https://raw.github.com/";
    Namespace   = "$rootnamespace$"         ;   // The downloaded content is wrapped in this namespace
    Includes    = new []
        {
            // Include the basic extension from T4Include
            Include (@"mrange/T4Include/master/Extensions/BasicExtensions.cs"),

            // Uncomment below to include dapper
            // Include (@"StackExchange/dapper-dot-net/master/Dapper%20NET40/SqlMapper.cs"),
        };
#>

<#@ include file="$(SolutionDir)\packages\T4Include.1.1.4\T4\IncludeWebFile.ttinclude" #>
