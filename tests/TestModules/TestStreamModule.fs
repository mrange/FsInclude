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

    let testNestedIntData (test : string -> int[][] -> 'T) =
        for variant in 0..5 do
            let data : int [][] =
                [|
                    for y in 1..variant ->
                        [|for x in 2..y -> x|]
                |]
            ignore <| test (sprintf "Nested, variant: %d" variant) data

    let testIntVariants (test : string -> int[] -> int -> 'T) =
        for description, data in intData do
            for variant in intVariants do
                ignore <| test description data variant

    let testIntDataPairs (test : string -> int[] -> string -> int[] -> 'T) =
        for description1, data1 in intData do
            for description2, data2 in intData do
                ignore <| test description1 data1 description2 data2

    let testOfVariant (name : string) (streamCreator : int[] -> Stream.Stream<int>) =
        testIntData <| fun description data ->
            let expected    = data
            let actual      = data |> streamCreator |> Stream.toArray
            eqf expected actual "%s: %A" name description

        testIntVariants <| fun description data limit ->
            let expected    = data.Take limit |> Seq.toArray
            let actual      = data |> streamCreator |> Stream.take limit |> Stream.toArray
            eqf expected actual "ofArray: %A - limit %d" description limit

        ()

    [<Test>]
    let ``testing Stream.all`` () =
        testIntVariants <| fun description data limit ->
            let test v = v > limit
            let expected    = data |> Seq.forall test
            let actual      = data |> Stream.ofArray |> Stream.all test
            eqf expected actual "all: %A - limit %d" description limit

        ()

    [<Test>]
    let ``testing Stream.any`` () =
        testIntVariants <| fun description data limit ->
            let test v = v > limit
            let expected    = data |> Seq.exists test
            let actual      = data |> Stream.ofArray |> Stream.any test
            eqf expected actual "any: %A - limit %d" description limit

        ()

    [<Test>]
    let ``testing Stream.append`` () =
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
    let ``testing Stream.choose`` () =
        testIntVariants <| fun description data limit ->
            let test v = if v > limit then Some (v.ToString()) else None
            let expected    = data |> Seq.choose test |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.choose test |> Stream.toArray
            eqf expected actual "choose: %A - limit %d" description limit

        ()

    [<Test>]
    let ``testing Stream.collect`` () =
        testNestedIntData <| fun descripton data ->
            let expected    =
                data
                |> Seq.collect (fun vs -> vs)
                |> Seq.toArray
            let actual      =
                data
                |> Stream.ofArray
                |> Stream.collect (fun vs -> vs |> Stream.ofArray)
                |> Stream.toArray
            eqf expected actual "collect: %A" descripton

        ()

    [<Test>]
    let ``testing Stream.concat`` () =
        testNestedIntData <| fun descripton data ->
            let expected    =
                data
                |> Seq.concat
                |> Seq.toArray
            let actual      =
                data
                |> Stream.ofArray
                |> Stream.map Stream.ofArray
                |> Stream.concat
                |> Stream.toArray
            eqf expected actual "concat: %A" descripton

        ()

    [<Test>]
    let ``testing Stream.delay`` () =
        testIntData <| fun description data ->
            let expected    = Seq.delay (fun () -> upcast data) |> Seq.toArray
            let actual      = Stream.delay (fun () -> data |> Stream.ofArray) |> Stream.toArray
            eqf expected actual "delay: %A" description

        ()

    [<Test>]
    let ``testing Stream.empty`` () =
        let expected    = [||]
        let actual      = Stream.empty |> Stream.toArray
        eq expected actual "empty"

        ()

    [<Test>]
    let ``testing Stream.enumerate`` () =
        testIntData <| fun description data ->
            let expected    = data |> Seq.mapi (fun i v -> i,v) |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.enumerate |> Stream.toArray
            eqf expected actual "enumerate: %A" description

        ()

    [<Test>]
    let ``testing Stream.filter`` () =
        testIntVariants <| fun description data limit ->
            let test v = v > limit
            let expected    = data |> Seq.filter test |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.filter test |> Stream.toArray
            eqf expected actual "filter: %A - limit %d" description limit

        ()

    [<Test>]
    let ``testing Stream.fold`` () =
        testIntData <| fun description data ->
            let f (s : int) (v : int) =
                s + v
            let expected    = data |> Seq.fold f 0
            let actual      = data |> Stream.ofArray |> Stream.fold f 0
            eqf expected actual "fold: %A" description

        ()

    [<Test>]
    let ``testing Stream.iter`` () =
        testIntData <| fun description data ->
            let expected    = ref 0
            let actual      = ref 0
            let f (s : int ref) (v : int) =
                s := !s + v
            data |> Seq.iter (f expected)
            data |> Stream.ofArray |> Stream.iter (f actual)
            eqf expected actual "iter: %A" description

        ()

    [<Test>]
    let ``testing Stream.isEmpty`` () =
        testIntData <| fun description data ->
            let expected    = data |> Seq.isEmpty
            let actual      = data |> Stream.ofArray |> Stream.isEmpty
            eqf expected actual "isEmpty: %A" description

        ()

    [<Test>]
    let ``testing Stream.map`` () =
        testIntVariants <| fun description data limit ->
            let map v = (v + limit).ToString ()
            let expected    = data |> Seq.map map |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.map map |> Stream.toArray
            eqf expected actual "map: %A - limit %d" description limit

        ()

    // TODO: ofRange

    [<Test>]
    let ``testing Stream.ofArray`` () =
        testOfVariant "ofArray" Stream.ofArray

        ()

    [<Test>]
    let ``testing Stream.ofList`` () =
        testOfVariant "ofList" (Seq.toList >> Stream.ofList)

        ()

    [<Test>]
    let ``testing Stream.ofSeq`` () =
        testOfVariant "ofSeq" Stream.ofSeq

        ()

    [<Test>]
    let ``testing Stream.skip`` () =
        testIntVariants <| fun description data limit ->
            let expected    = data.Skip limit |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.skip limit |> Stream.toArray
            eqf expected actual "skip: %A - limit %d" description limit

        ()

    [<Test>]
    let ``testing Stream.singleton`` () =
        let expected    = [|4|]
        let actual      = 4 |> Stream.singleton |> Stream.toArray

        eq expected actual "singleTon"

        ()

    [<Test>]
    let ``testing Stream.take`` () =
        testIntVariants <| fun description data limit ->
            let expected    = data.Take limit |> Seq.toArray
            let actual      = data |> Stream.ofArray |> Stream.take limit |> Stream.toArray
            eqf expected actual "take: %A - limit %d" description limit

        ()

    [<Test>]
    let ``testing Stream.toArray`` () =
        testIntData <| fun description data ->
            let expected    = data
            let actual      = data |> Stream.ofArray |> Stream.toArray
            eqf expected actual "toArray: %A - limit %d" description

        ()

    [<Test>]
    let ``testing Stream.toList`` () =
        testIntData <| fun description data ->
            let expected    = data |> Seq.toList
            let actual      = data |> Stream.ofArray |> Stream.toList
            eqf expected actual "toList: %A - limit %d" description

        ()

    [<Test>]
    let ``testing Stream.toSum`` () =
        testIntData <| fun description data ->
            let expected    = data |> Seq.sum
            let actual      = data |> Stream.ofArray |> Stream.toSum 0
            eqf expected actual "toSum: %A - limit %d" description

        ()

        testIntData <| fun description data ->
            let expected    = 10 + (data |> Seq.sum)
            let actual      = data |> Stream.ofArray |> Stream.toSum 10
            eqf expected actual "toSum: %A - limit %d" description

        ()

    [<Test>]
    let ``testing basic Stream use cases`` () =
        let series f t = (t - f + 1)*(t + f) / 2

        let test f t sum =
            eqf (series f t) sum "Sum %d..%d" f t

        let sum =
            Stream.ofRange 1 1 101
            |> Stream.toSum 0

        test 1 100 sum

        let sum =
            Stream.ofRange 1 1 101
            |> Stream.take 10
            |> Stream.toSum 0

        test 1 10 sum

        let sum =
            Stream.ofRange 1 1 101
            |> Stream.skip 89
            |> Stream.toSum 0

        test 90 100 sum


        ()

#if DEBUG
#else
    [<Test>]
    let ``testing Stream performance`` () =
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

            let linqSum () = ((data.Where(fun x -> x % 2L = 0L)).Select(fun x -> x + 1L)).Sum()

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

            let rstreamSum () =
                RStream.ofArray data (RStream.filter (fun x -> x % 2L = 0L) (RStream.map (fun x -> x + 1L) (RStream.toSum 0L)))


            let testCases =
                [|
                    "Seq"       , seqSum
                    "For"       , forSum
                    "LINQ"      , linqSum
                    "Stream"    , streamSum
                    "RStream"   , rstreamSum
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
#endif
