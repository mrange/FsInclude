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
        member x.WriteFlow (bytes, start, length) : Multiplexer<unit> =
            let ar = x.BeginWrite (bytes, start, length, null, null)
            Multiplexer.adaptLegacyAsync ar x.EndWrite

    type HttpListener with
        member x.GetContextFlow () : Multiplexer<HttpListenerContext> =
            let ar = x.BeginGetContext (null, null)
            Multiplexer.adaptLegacyAsync ar x.EndGetContext

[<EntryPoint>]
let main argv =

    let seq = ref 0
    let rnd = Random 1974031

    let next ()     = 
        seq := !seq + 1
        !seq
    let random ()   = rnd.NextDouble ()

    let delay (d : int) : Multiplexer<unit> =
        Multiplexer.adaptUnitTask (Task.Delay d)

    let writeBytes (batchSize : int) (stream : Stream) (bytes : byte[]) : Multiplexer<unit> =
        multiplexer {
            let batch = Array.zeroCreate batchSize
            let e = bytes.Length - 1
            for i in 0..batchSize..e do
                let length = min batchSize (bytes.Length - i)
                Array.Copy (bytes, i, batch, 0, length)

                do! stream.WriteFlow (batch, 0, length)
        }

    let webRequest (context : HttpListenerContext) =
        multiplexer {
            try
                let req = context.Request
                use res = context.Response
                use out = res.OutputStream

                printfn "Web request from: %A" req.UserHostAddress

                let dt  = DateTime.Now

                let d   = int <| random () * 5000.
                let s   = next ()

                do! delay d

                let str     = sprintf "<html><title>Hello</title><body>Seq: %d, Delay: %d, Then: %A, Now: %A</body></html>" s d dt DateTime.Now
                let bytes   = System.Text.Encoding.UTF8.GetBytes str

                res.ContentLength64 <- bytes.LongLength
                do! writeBytes 25 out bytes
            with
            | e -> printfn "WebRequest: Caught exception: %A" e.Message


            return ()
        }


    let webServer =
        multiplexer {
            use listener = new HttpListener ()
            let prefixes = [|"http://localhost:8080/"|]

            listener.Start ()

            for prefix in prefixes do
                listener.Prefixes.Add prefix

            printfn "Web server started..."

            while true do
                let! context    = listener.GetContextFlow ()

                let! child      = webRequest context |> Multiplexer.startChild

                ()

            return 0
        }


    Multiplexer.run webServer
