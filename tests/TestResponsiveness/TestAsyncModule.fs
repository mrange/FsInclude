﻿// Copyright 2015 Mårten Rånge
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

module internal TestGem =

    open System.Diagnostics
    open System.Threading.Tasks

    open FsInclude
    open FsInclude.Gem
    open FsInclude.Test

    let time (action : unit -> 'T) =
        let sw = Stopwatch ()

        sw.Start ()

        let result = action ()
        
        sw.Stop ()

        sw.ElapsedMilliseconds, result

    
    let ex = System.Exception ()

    let throwingFlow b = 
        flow {
            if b then 
                raise ex
            else ()

            return b
        }

    let getCounters () =
        let r  = ref 0
        let r1 = ref 0
        let r2 = ref 0
        let r3 = ref 0

        let next () =
            r := !r + 1
            !r

        let clear () = 
            r := 0
            r1 := 0
            r2 := 0
            r3 := 0

        r,r1,r2,r3,next,clear

    type SetOnDispose(nm : string, ri : int ref, next : unit -> int) =
        inherit BaseDisposable()

        override x.OnDispose () = 
            printfn "Disposed: %s" nm
            ri := next ()


    [<Test>]
    let ``test basic flow`` () =
        let f =
            flow {
                return 1
            }

        let actual = Flow.Run f

        eq 1 actual "Basic flow"

    [<Test>]
    let ``test delayed flow`` () =
        let elapsed, actual = time <| fun () ->
            let t = (Task.Delay 200).ContinueWith(fun _ -> 2)

            let f =
                flow {
                    let! r = Flow.AdaptTask t
                    return r + 1
                }

            Flow.Run f

        eq 3 actual "Delayed flow"
        range 200L 220L elapsed "Delayed flow"

    [<Test>]
    let ``test child delayed flow`` () =
        let elapsed, actual = time <| fun () ->
            let t1 = (Task.Delay 100).ContinueWith(fun _ -> 2)
            let t2 = (Task.Delay 200).ContinueWith(fun _ -> 4)

            let f =
                flow {
                    let! r1a = Flow.AdaptTask t1 |> Flow.StartChild
                    let! r2  = Flow.AdaptTask t2
                    let! r1  = r1a 
                    return r1 + r2 + 1
                }

            Flow.Run f

        eq 7 actual "Child delayed flow"
        range 200L 220L elapsed "Child delayed flow"

        ()

    [<Test>]
    let ``test try...finally`` () =
        let r,r1,r2,r3,next,clear = getCounters ()

        let f b =
            flow {
                clear ()

                let result = ref false

                try
                    try
                        let! i = throwingFlow b
                        result := i
                    finally
                        r1 := next ()
                finally
                    r2 := next ()
                r3 := next ()

                return !result
            }

        let actual = Flow.Run (f false)

        eq false actual                         "try..finally - success, actual"
        eq 1 !r1                                "try..finally - success, r1"
        eq 2 !r2                                "try..finally - success, r2"
        eq 3 !r3                                "try..finally - success, r3"

        eqexn ex (fun () -> Flow.Run (f true))  "try..finally - failure, actual"
        eq 1 !r1                                "try..finally - failure, r1"
        eq 2 !r2                                "try..finally - failure, r2"
        eq 0 !r3                                "try..finally - failure, r3"

        ()


    [<Test>]
    let ``test try...with`` () =
        let r,r1,r2,r3,next,clear = getCounters ()

        let f b =
            flow {
                clear ()

                let result = ref false

                try
                    try
                        let! i = throwingFlow b
                        result := i
                    with
                    | e ->
                        r1 := next ()
                        raise e
                with
                | e ->
                    r2 := next ()
                    raise e
                r3 := next ()

                return !result
            }

        let actual = Flow.Run (f false)

        eq false actual                         "try..with - success, actual"
        eq 0 !r1                                "try..with - success, r1"
        eq 0 !r2                                "try..with - success, r2"
        eq 1 !r3                                "try..with - success, r3"

        eqexn ex (fun () -> Flow.Run (f true))  "try..with - failure, actual"
        eq 1 !r1                                "try..with - failure, r1"
        eq 2 !r2                                "try..with - failure, r2"
        eq 0 !r3                                "try..with - failure, r3"

        ()

    [<Test>]
    let ``test use && use!`` () =
        let r,r1,r2,r3,next,clear = getCounters ()

        let t =
            flow {
                return new SetOnDispose ("r1", r1, next)
            }

        let u b = 
            flow {
                let result = ref false

                use i2 = new SetOnDispose ("r2", r2, next)
                use! i1 = t
                let! i = throwingFlow b
                return i
            }

        let f b =
            flow {
                clear ()

                let! result = u b
                r3 := next ()

                return result
            }

        let actual = Flow.Run (f false)

        eq false actual                         "use - success, actual"
        eq 1 !r1                                "use - success, r1"
        eq 2 !r2                                "use - success, r2"
        eq 3 !r3                                "use - success, r3"

        eqexn ex (fun () -> Flow.Run (f true))  "use - failure, actual"
        eq 1 !r1                                "use - failure, r1"
        eq 2 !r2                                "use - failure, r2"
        eq 0 !r3                                "use - failure, r3"

        ()