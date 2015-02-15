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

module internal TestMultiplex =

    open System.Diagnostics
    open System.Threading.Tasks

    open FsInclude
    open FsInclude.Multiplex
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
        let r4 = ref 0

        let next () =
            r := !r + 1
            !r

        let clear () =
            r := 0
            r1 := 0
            r2 := 0
            r3 := 0
            r4 := 0

        r,r1,r2,r3,r4,next,clear

    let delayedTask (d : int) v = (Task.Delay d).ContinueWith(fun _ -> v)

    type SetOnDispose(nm : string, ri : int ref, next : unit -> int) =
        inherit BaseDisposable()

        override x.OnDispose () =
//            printfn "Disposed: %s" nm
            ri := next ()


    [<Test>]
    let ``test return and return!`` () =
        let f =
            flow {
                return 1
            }

        let actual = Flow.Run f

        eq 1 actual "return"

        let ff =
            flow {
                return! f
            }

        let actual = Flow.Run ff

        eq 1 actual "return!"

        ()

    [<Test>]
    let ``test bind`` () =
        let r,r1,r2,r3,r4,next,clear = getCounters ()

        let t (r : int ref) =
            flow {
                r := next ()
                return !r
            }

        let f =
            flow {
                let! a = (t r1)
                let! b = (t r2)
                return a + b
            }

        let actual = Flow.Run f

        eq 3 actual "bind"
        eq 1 !r1    "bind, r1"
        eq 2 !r2    "bind, r2"

        ()

    [<Test>]
    let ``test combine`` () =
        let r,r1,r2,r3,r4,next,clear = getCounters ()

        let t (r : int ref) =
            flow {
                r := next ()
                return ()
            }

        let f =
            flow {
                do! (t r1)
                do! (t r2)
                return !r1 + !r2
            }

        let actual = Flow.Run f

        eq 3 actual "combine"
        eq 1 !r1    "combine, r1"
        eq 2 !r2    "combine, r2"

        ()

    [<Test>]
    let ``test for`` () =
        let r,r1,r2,r3,r4,next,clear = getCounters ()

        let t (r : int ref) =
            flow {
                r := next ()
                return !r
            }

        let f =
            flow {
                let sum = ref 0
                for x in 0..3 do
                    let! v = t r1
                    sum := !sum + x + v

                return !sum
            }

        let actual = Flow.Run f

        eq 16 actual    "for"
        eq 4 !r1        "for, r1"

        ()

    [<Test>]
    let ``test while`` () =
        let r,r1,r2,r3,r4,next,clear = getCounters ()

        let t (r : int ref) =
            flow {
                r := next ()
                return !r
            }

        let f =
            flow {
                let sum = ref 0
                while !r1 < 4 do
                    let! v = t r1
                    sum := !sum + v

                return !sum
            }

        let actual = Flow.Run f

        eq 10 actual    "while"
        eq 4 !r1        "while, r1"

        ()

    [<Test>]
    let ``test delayed flow`` () =
        let elapsed, actual = time <| fun () ->
            let f =
                flow {
                    let! r = Flow.AdaptTask (delayedTask 200 2)
                    return r + 1
                }

            Flow.Run f

        eq 3 actual "Delayed flow"
        range 190L 240L elapsed "Delayed flow"

    [<Test>]
    let ``test child delayed flow`` () =
        let elapsed, actual = time <| fun () ->
            let f =
                flow {
                    let! r1a = Flow.AdaptTask (delayedTask 100 2) |> Flow.StartChild
                    let! r2  = Flow.AdaptTask (delayedTask 200 4)
                    let! r1  = r1a
                    return r1 + r2 + 1
                }

            Flow.Run f

        eq 7 actual "Child delayed flow"
        range 190L 240L elapsed "Child delayed flow"

        ()

    [<Test>]
    let ``test try...finally`` () =
        let r,r1,r2,r3,r4,next,clear = getCounters ()

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
        let r,r1,r2,r3,r4,next,clear = getCounters ()

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
        let r,r1,r2,r3,r4,next,clear = getCounters ()

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

                use i3 = new SetOnDispose ("r3", r3, next)
                let! result = u b

                r4 := next ()

                return result
            }

        let actual = Flow.Run (f false)

        eq false actual                         "use - success, actual"
        eq 1 !r1                                "use - success, r1"
        eq 2 !r2                                "use - success, r2"
        eq 4 !r3                                "use - success, r3"
        eq 3 !r4                                "use - success, r4"

        eqexn ex (fun () -> Flow.Run (f true))  "use - failure, actual"
        eq 1 !r1                                "use - failure, r1"
        eq 2 !r2                                "use - failure, r2"
        eq 3 !r3                                "use - failure, r3"
        eq 0 !r4                                "use - failure, r4"

        ()

    [<Test>]
    let ``test handlers with child tasks`` () =
        let r,r1,r2,r3,r4,next,clear = getCounters ()

        let t (nm : string) (ir : int ref) (d : int) (r : int) (v : int) =
            flow {
                use disp = new SetOnDispose (nm, ir, next)
                let task = delayedTask d r
                let! tt = Flow.AdaptTask task

                if (v &&& r) <> 0 then
                    raise ex

                return tt
            }

        let f v =
            flow {
                clear ()

                use disp = new SetOnDispose ("r3", r3, next)

                let! t1a = t "r1" r1 10  1 v |> Flow.StartChild
                let! t2a = t "r2" r2 100 2 v |> Flow.StartChild

                let! t1 = t1a
                let! t2 = t2a

                r4 := next ()

                return t1 + t2
            }

        let actual = Flow.Run (f 0)

        eq 3 actual                             "child handlers - variant 0"
        eq 1 !r1                                "child handlers - variant 0, r1"
        eq 2 !r2                                "child handlers - variant 0, r2"
        eq 4 !r3                                "child handlers - variant 0, r3"
        eq 3 !r4                                "child handlers - variant 0, r4"


        eqexn ex (fun () -> Flow.Run (f 1))     "child handlers - variant 1"
        eq 1 !r1                                "child handlers - variant 1, r1"
        eq 3 !r2                                "child handlers - variant 1, r2"
        eq 2 !r3                                "child handlers - variant 1, r3"
        eq 0 !r4                                "child handlers - variant 1, r4"

        eqexn ex (fun () -> Flow.Run (f 2))     "child handlers - variant 2"
        eq 1 !r1                                "child handlers - variant 2, r1"
        eq 2 !r2                                "child handlers - variant 2, r2"
        eq 3 !r3                                "child handlers - variant 2, r3"
        eq 0 !r4                                "child handlers - variant 2, r4"

        eqexn ex (fun () -> Flow.Run (f 3))     "child handlers - variant 3"
        eq 1 !r1                                "child handlers - variant 3, r1"
        eq 3 !r2                                "child handlers - variant 3, r2"
        eq 2 !r3                                "child handlers - variant 3, r3"
        eq 0 !r4                                "child handlers - variant 3, r4"

        ()
