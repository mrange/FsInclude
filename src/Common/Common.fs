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

namespace FsInclude

type LogLevel =
    | Info
    | Warning
    | Error
    | Exception

type ILog =
    interface
        abstract LogMessage: LogLevel*string -> unit
    end

module Log =

    open Microsoft.FSharp.Core.Printf
    open System.Text

    let empty =
        {
            new ILog with
                member x.LogMessage (ll, msg) =
                    let prelude =
                        match ll with
                        | Info      -> "INFO      : "
                        | Warning   -> "WARNING   : "
                        | Error     -> "ERROR     : "
                        | Exception -> "EXCEPTION : "
                    let sb = StringBuilder (prelude)
                    ignore <| sb.Append msg
                    System.Diagnostics.Trace.WriteLine <| sb.ToString ()
        }

    let mutable log = empty

    let info (message : string) : unit =
        log.LogMessage (LogLevel.Info, message)

    let infof (format : StringFormat<'T, unit>) : 'T =
        ksprintf info format

    let warning (message : string) : unit =
        log.LogMessage (LogLevel.Warning, message)

    let warningf (format : StringFormat<'T, unit>) : 'T =
        ksprintf warning format

    let error (message : string) : unit =
        log.LogMessage (LogLevel.Error, message)

    let errorf (format : StringFormat<'T, unit>) : 'T =
        ksprintf error format
