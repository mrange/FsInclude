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

open System
open System.IO
open System.Net
open System.Threading.Tasks

open Included.FsInclude.Multiplex

[<AutoOpen>]
module internal Extensions =
    type Stream with
        member x.WriteFlow (bytes, start, length) : Flow<unit> =
            let ar = x.BeginWrite (bytes, start, length, null, null)
            Flow.adaptLegacyAsync ar x.EndWrite

    type HttpListener with
        member x.GetContextFlow () : Flow<HttpListenerContext> =
            let ar = x.BeginGetContext (null, null)
            Flow.adaptLegacyAsync ar x.EndGetContext

[<EntryPoint>]
let main argv =

    let delay (d : int) : Flow<unit> =
        Flow.adaptUnitTask (Task.Delay d)

    let writeBytes (batchSize : int) (stream : Stream) (bytes : byte[]) : Flow<unit> =
        flow {
            let batch = Array.zeroCreate batchSize
            let e = bytes.Length - 1
            for i in 0..batchSize..e do
                let length = min batchSize (bytes.Length - i)
                Array.Copy (bytes, i, batch, 0, length)

                do! stream.WriteFlow (batch, 0, length)
        }

    let webRequest (context : HttpListenerContext) =
        flow {
            try
                let req = context.Request
                use res = context.Response
                use out = res.OutputStream

                printfn "Web request from: %A" req.UserHostAddress

                let dt  = DateTime.Now

                do! delay 5000

                let str     = sprintf "<html><title>Hello</title><body>%A,%A</body></html>" dt DateTime.Now
                let bytes   = System.Text.Encoding.UTF8.GetBytes str

                res.ContentLength64 <- bytes.LongLength
                do! writeBytes 25 out bytes
            with
            | e -> printfn "WebRequest: Caught exception: %A" e.Message


            return ()
        }


    let webServer =
        flow {
            use listener = new HttpListener ()
            let prefixes = [|"http://localhost:8080/"|]

            listener.Start ()

            for prefix in prefixes do
                listener.Prefixes.Add prefix

            printfn "Web server started..."

            while true do
                let! context    = listener.GetContextFlow ()

                let! child      = webRequest context |> Flow.startChild

                ()

            return 0
        }


    Flow.run webServer
