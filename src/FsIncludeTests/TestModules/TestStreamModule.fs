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

    let timeIt (action : unit -> 'T) = 
        let sw = Stopwatch ()
        sw.Start ()

        let res = action ()

        sw.Stop ()

        sw.ElapsedMilliseconds, res

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
        let sum =         
            RStream.ofRange 0 1 100
            |> RStream.toSum 0

        eq 4950 sum

        ()

//    [<Test>]
    let testPerf () =

        let total = 200000000
        let outer = 10000
        let inner = total / outer

        let linqSum () = 
            (Enumerable.Range (0,inner)).Sum ()

        let seqSum () = 
            Enumerable.Range (0,inner) |> Seq.sum

        let rstreamSum () = 
            RStream.ofRange 0 1 inner |> RStream.toSum 0

        let fstreamSum () = 
            FStream.ofRange 0 1 inner |> FStream.toSum 0

        let linq_elapsed    , linq_result       = perfTest outer linqSum
        let seq_elapsed     , seq_result        = perfTest outer seqSum
        let rstream_elapsed , rstream_result    = perfTest outer rstreamSum
        let fstream_elapsed , fstream_result    = perfTest outer rstreamSum

        eq linq_result seq_result
        eq linq_result rstream_result
        eq linq_result fstream_result

        Test.infof "linq_elapsed: %A, seq_elapsed: %A, rstream_elapsed: %A, fstream_elapsed: %A" linq_elapsed seq_elapsed rstream_elapsed fstream_elapsed

        ()

//    [<Test>]
    let testPerf2 () =
        let data = [|1..10000000|] |> Array.map int64

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

        let rstreamSum () = 
            data
            |> RStream.ofArray
            |> RStream.filter (fun x -> x % 2L = 0L)
            |> RStream.map (fun x -> x + 1L)
            |> RStream.toSum 0L

        let fstreamSum () = 
            data
            |> FStream.ofArray
            |> FStream.filter (fun x -> x % 2L = 0L)
            |> FStream.map (fun x -> x + 1L)
            |> FStream.toSum 0L

        let seq_elapsed     , _ = timeIt seqSum
        let array_elapsed   , _ = timeIt arraySum
        let rstream_elapsed , _ = timeIt rstreamSum
        let fstream_elapsed , _ = timeIt fstreamSum

        Test.infof "linq_elapsed: %A, array_elapsed: %A, rstream_elapsed: %A, fstream_elapsed: %A" seq_elapsed array_elapsed rstream_elapsed fstream_elapsed

        ()

    [<Test>]
    let testPerf3 () =
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

            let rstreamSum () = 
                data
                |> RStream.ofArray
                |> RStream.filter (fun x -> x % 2L = 0L)
                |> RStream.map (fun x -> x + 1L)
                |> RStream.toSum 0L

            let fstreamSum () = 
                data
                |> FStream.ofArray
                |> FStream.filter (fun x -> x % 2L = 0L)
                |> FStream.map (fun x -> x + 1L)
                |> FStream.toSum 0L

            let seq_elapsed     , _ = perfTest outer seqSum
            let array_elapsed   , _ = perfTest outer arraySum
            let rstream_elapsed , _ = perfTest outer rstreamSum
            let fstream_elapsed , _ = perfTest outer fstreamSum

            Test.infof 
                "%d, %d: linq_elapsed: %A, array_elapsed: %A, rstream_elapsed: %A, fstream_elapsed: %A" 
                outer
                inner
                seq_elapsed 
                array_elapsed 
                rstream_elapsed 
                fstream_elapsed

        ()


(*
        let fstreamSum () = 
            data
            |> FStream.ofArray
            |> FStream.filter (fun x -> x % 2L = 0L)
            |> FStream.map (fun x -> x + 1L)
            |> FStream.sum


*)