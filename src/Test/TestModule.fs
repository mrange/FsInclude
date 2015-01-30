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

module internal Test =
    open System
    open System.Reflection
    open System.Text

    open Microsoft.FSharp.Core.Printf

    [<AttributeUsage(AttributeTargets.Method)>]
    [<AllowNullLiteral>]
    [<Sealed>]
    type TestAttribute() =
        inherit Attribute()

    let mutable failureCount = 0

    let colorprint (cc : ConsoleColor) (prelude : string) (msg : string) =
        let old = Console.ForegroundColor
        Console.ForegroundColor <- cc
        try
            Console.Write prelude
            Console.WriteLine msg
        finally
            Console.ForegroundColor <- old


    let success     = colorprint ConsoleColor.Green "SUCCESS   : "
    let highlight   = colorprint ConsoleColor.White "HIGHLIGHT : "
    let info        = colorprint ConsoleColor.Gray  "INFO      : "
    let fail        = colorprint ConsoleColor.Red   "FAILURE   : "

    let successf (format : StringFormat<'T, unit>) : 'T =
        ksprintf success format

    let highlightf (format : StringFormat<'T, unit>) : 'T =
        ksprintf highlight format

    let infof (format : StringFormat<'T, unit>) : 'T =
        ksprintf info format

    let failf (format : StringFormat<'T, unit>) : 'T =
        ksprintf fail format

    let eq (expected : 'T) (actual : 'T) : bool =
        if expected = actual then
            true
        else
            failf "EXPECTED_EQ: %A = %A" expected actual
            false

    let lte (expected : 'T) (actual : 'T) : bool =
        if expected <= actual then
            true
        else
            failf "EXPECTED_LTE: %A <= %A" expected actual
            false

    let gte (expected : 'T) (actual : 'T) : bool =
        if expected >= actual then
            true
        else
            failf "EXPECTED_GT: %A >= %A" expected actual
            false


    let runTestCases (assembly : Assembly) : bool =
        failureCount <- 0

        let testMethods =
            assembly.GetTypes ()
            |> Seq.collect (fun t -> t.GetMethods (BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic))
            |> Seq.filter (fun mi -> mi.GetCustomAttribute<TestAttribute> () <> null)
            |> Seq.toArray

        for testMethod in testMethods do
            try
                try
                    infof "BEGIN_TESTCASE: %s.%s" testMethod.DeclaringType.Name testMethod.Name
                    ignore <| testMethod.Invoke (null, null)
                with
                | e -> failf "EXCEPTION: %A" e
            finally
                infof "END_TESTCASE: %s.%s" testMethod.DeclaringType.Name testMethod.Name

        if failureCount > 0 then
            failf "%d tests failed" failureCount
            false
        else
            success "All tests passed"
            true
