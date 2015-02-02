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
#nowarn "25"    // Test functions patterns are not complete but it's ok

namespace FsInclude.Tests

module internal TestBasicModule =

    open FsInclude
    open FsInclude.Test

    let generateTestCases (n : int) : (int option) [][] =
        let generate t i =
            let b = 1 <<< i
            if (t &&& b) = b then
                Some (i + 1)
            else
                None

        let variants = 1 <<< n

        [|
            for i in 0..(variants - 1) ->
                Array.init n (generate i)
        |]

    let liftTestCases (n : int) (test : string -> int option -> (int option) [] -> 'T) =
        let testCases   = generateTestCases n
        for testCase in testCases do
            let f s v =
                match s,v with
                | Some ss, Some vv  -> Some (ss + vv)
                | _                 -> None

            let expected =
                testCase
                |> Array.fold f (Some 0)

            ignore <| test (sprintf "%A" testCase) expected testCase


    [<Test>]
    let ``test Opt.valueOrDefault`` () =

        let expected    = 1
        let actual      = (Some 1) |> Opt.valueOrDefault 2

        eq expected actual "valueOrDefault: Some"

        let expected    = 2
        let actual      = None |> Opt.valueOrDefault 2

        eq expected actual "lift1: None"

        ()

    [<Test>]
    let ``test Opt.lift1`` () =
        liftTestCases 1 <| fun description expected testCase ->
            let [|i0|] = testCase

            let actual = Opt.lift1 (fun v0 -> v0) i0

            eqf expected actual "lift1: variant - %A" testCase

    [<Test>]
    let ``test Opt.lift2`` () =
        liftTestCases 2 <| fun description expected testCase ->
            let [|i0;i1|] = testCase

            let actual = Opt.lift2 (fun v0 v1 -> v0 + v1) i0 i1

            eqf expected actual "lift2: variant - %A" testCase

        ()

    [<Test>]
    let ``test Opt.lift3`` () =
        liftTestCases 3 <| fun description expected testCase ->
            let [|i0;i1;i2|] = testCase

            let actual = Opt.lift3 (fun v0 v1 v2 -> v0 + v1 + v2) i0 i1 i2

            eqf expected actual "lift3: variant - %A" testCase

        ()

    [<Test>]
    let ``test Opt.lift4`` () =
        liftTestCases 4 <| fun description expected testCase ->
            let [|i0;i1;i2;i3|] = testCase

            let actual = Opt.lift4 (fun v0 v1 v2 v3-> v0 + v1 + v2 + v3) i0 i1 i2 i3

            eqf expected actual "lift4: variant - %A" testCase

        ()

    [<Test>]
    let ``test Opt.lift5`` () =
        liftTestCases 5 <| fun description expected testCase ->
            let [|i0;i1;i2;i3;i4|] = testCase

            let actual = Opt.lift5 (fun v0 v1 v2 v3 v4 -> v0 + v1 + v2 + v3 + v4) i0 i1 i2 i3 i4

            eqf expected actual "lift5: variant - %A" testCase

        ()
