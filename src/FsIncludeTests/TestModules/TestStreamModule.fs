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

module internal TestStreamModule =

    open FsInclude
    open FsInclude.Test

    open System.Diagnostics
    open System.Linq

    let perfTest (count : int) (action : unit -> 'T) =
        let sw = Stopwatch ()
        sw.Start ()

        let res = action ()

        for i in 2..count do
            ignore <| action ()

        sw.Stop ()

        sw.ElapsedMilliseconds, res

    let intData =
        [|
            "No ints"   , [||]
            "1 int"     , [|100|]
            "10 ints"   , [|3;1;4;1;5;9;2;6;5;4|]
        |]

    let intVariants = [|0;5;10|]

    let testIntData (test : string -> int[] -> 'T) =
        for description, data in intData do
                ignore <| test description data

    let testIntVariants (test : string -> int[] -> int -> 'T) =
        for description, data in intData do
            for variant in intVariants do
                ignore <| test description data variant

    let testIntDataPairs (test : string -> int[] -> string -> int[] -> 'T) =
        for description1, data1 in intData do
            for description2, data2 in intData do
                ignore <| test description1 data1 description2 data2

    [<Test>]
    let testAll () =
        testIntVariants <| fun description data limit ->
            let test v = v > limit
            let expected    = data |> Seq.forall test
            let actual      = data |> Stream.ofArray |> Stream.all test
            eqf expected actual "all: %A - limit %d" description limit

        ()

    [<Test>]
    let testAny () =
        testIntVariants <| fun description data limit ->
            let test v = v > limit
            let expected    = data |> Seq.exists test
            let actual      = data |> Stream.ofArray |> Stream.any test
            eqf expected actual "any: %A - limit %d" description limit

        ()

    [<Test>]
    let testAppend () =
        testIntDataPairs <| fun description1 data1 description2 data2 ->
            let expected    =
                data1
                |> Seq.append data2
                |> Seq.toArray
            let actual      =
                data1 |> Stream.ofArray
                |> Stream.append (data2 |> Stream.ofArray)
                |> Stream.toArray
            eqf expected actual "append: %A * %A" description1 description2

        ()

    [<Test>]
    let testChoose () =
        testIntVariants <| fun description data limit ->
            let test v = if v > limit then Some (v.ToString()) else None
            let expected    = data |> Seq.choose test |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.choose test |> Stream.toArray
            eqf expected actual "choose: %A - limit %d" description limit

        ()

    // TODO: collect
    // TODO: concat

    [<Test>]
    let testDelay () =
        testIntData <| fun description data ->
            let expected    = Seq.delay (fun () -> upcast data) |> Seq.toArray
            let actual      = Stream.delay (fun () -> data |> Stream.ofArray) |> Stream.toArray
            eqf expected actual "delay: %A" description

        ()

    [<Test>]
    let testEmpty () =
        let expected    = [||]
        let actual      = Stream.empty |> Stream.toArray
        eq expected actual "empty"

        ()

    [<Test>]
    let testEnumerate () =
        testIntData <| fun description data ->
            let expected    = data |> Seq.mapi (fun i v -> i,v) |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.enumerate |> Stream.toArray
            eqf expected actual "enumerate: %A" description

        ()

    [<Test>]
    let testFilter () =
        testIntVariants <| fun description data limit ->
            let test v = v > limit
            let expected    = data |> Seq.filter test |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.filter test |> Stream.toArray
            eqf expected actual "filter: %A - limit %d" description limit

        ()

    // TODO: fold
    // TODO: iter
    // TODO: isEmpty

    [<Test>]
    let testMap () =
        testIntVariants <| fun description data limit ->
            let map v = (v + limit).ToString ()
            let expected    = data |> Seq.map map |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.map map |> Stream.toArray
            eqf expected actual "map: %A - limit %d" description limit

        ()

    // TODO: ofRange

    [<Test>]
    let testOfArray () =
        testIntData <| fun description data ->
            let expected    = data
            let actual      = data |> Stream.ofArray |> Stream.toArray
            eqf expected actual "ofArray: %A" description

        testIntVariants <| fun description data limit ->
            let expected    = data |> Seq.take limit |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.take limit |> Stream.toArray
            eqf expected actual "ofArray: %A - limit %d" description limit

        ()

    [<Test>]
    let testOfList () =
        testIntData <| fun description data ->
            let expected    = data
            let actual      = data |> Seq.toList |> Stream.ofList |> Stream.toArray
            eqf expected actual "ofArray: %A" description

        testIntVariants <| fun description data limit ->
            let expected    = data |> Seq.take limit |> Seq.toArray
            let actual      = data |> Seq.toList |> Stream.ofList |> Stream.take limit |> Stream.toArray
            eqf expected actual "ofArray: %A - limit %d" description limit

        ()

    [<Test>]
    let testStreams () =
        let series f t = (t - f + 1)*(t + f) / 2

        let test f t sum =
            eqf (series f t) sum "Sum %d..%d" f t

        let sum =
            Stream.ofRange 0 1 101
            |> Stream.toSum 0

        test 0 100 sum

        let sum =
            Stream.ofRange 0 1 101
            |> Stream.take 10
            |> Stream.toSum 0

        test 0 10 sum

        let sum =
            Stream.ofRange 0 1 101
            |> Stream.skip 90
            |> Stream.toSum 0

        test 90 100 sum


        ()

    [<Test>]
    let testPerf () =
        let total = 10000000
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
                eqf seq_result actual "Reslut for %A" name
                gtef seq_elapsed elapsed "Performance for %A" name

        ()
