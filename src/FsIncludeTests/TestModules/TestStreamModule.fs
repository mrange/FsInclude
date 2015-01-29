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

#nowarn "20"    // Test functions returns bools which we often don't care about

namespace FsInclude.Tests

module TestStreamModule =

    open FsInclude
    open FsInclude.Test

    open System.Diagnostics
    open System.Linq

    let empty = [||]

    let perfTest (count : int) (action : unit -> 'T) =
        let sw = Stopwatch ()
        sw.Start ()

        let res = action ()

        for i in 2..count do
            ignore <| action ()

        sw.Stop ()

        sw.ElapsedMilliseconds, res



    [<Test>]
    let testStreams () =
        let series f t = (t - f + 1)*(t + f) / 2

        let sum =
            Stream.ofRange 0 1 101
            |> Stream.toSum 0

        eq (series 0 100) sum

        let sum =
            Stream.ofRange 0 1 101
            |> Stream.take 10
            |> Stream.toSum 0

        eq (series 0 10) sum

        let sum =
            Stream.ofRange 0 1 101
            |> Stream.skip 90
            |> Stream.toSum 0

        eq (series 90 100) sum


        ()

    [<Test>]
    let testPerf () =
        let total = 50000000
        let outers=
            [|
                10
                1000
                100000
                total / 5
            |]

        for outer in outers do
            let inner = total / outer

            let data = [|1..inner|] |> Array.map int64

            highlightf "Performance run: Total: %d, Outer: %d, Inner: %d" total outer inner


            let forSum () =
                let mutable sum = 0L
                for x in data do
                    if x % 2L = 0L then
                        sum <- sum + (x + 1L)
                sum

            let seqSum () =
                data
                |> Seq.filter (fun x -> x % 2L = 0L)
                |> Seq.map (fun x -> x + 1L)
                |> Seq.sum

            let arraySum () =
                data
                |> Array.filter (fun x -> x % 2L = 0L)
                |> Array.map (fun x -> x + 1L)
                |> Array.sum

            let streamSum () =
                data
                |> Stream.ofArray
                |> Stream.filter (fun x -> x % 2L = 0L)
                |> Stream.map (fun x -> x + 1L)
                |> Stream.toSum 0L

            let testCases =
                [|
                    "Seq"       , seqSum
                    "For"       , forSum
                    "Stream"    , streamSum
                |]

            let testResults =
                testCases
                |> Array.map (fun (name, test) ->
                    let elapsed, result = perfTest outer test
                    name, elapsed, result
                    )

            let _, seq_elapsed, seq_result  = testResults.[0]

            for name, elapsed, actual in testResults do
                infof "TestResult for: %A - Time: %d ms" name elapsed
                eq seq_result actual
                gte seq_elapsed elapsed

        ()


(*
        let fstreamSum () =
            data
            |> FStream.ofArray
            |> FStream.filter (fun x -> x % 2L = 0L)
            |> FStream.map (fun x -> x + 1L)
            |> FStream.sum


*)