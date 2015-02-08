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

module internal TestGem =

    open System.Diagnostics
    open System.Threading.Tasks

    open FsInclude.Gem
    open FsInclude.Test

    let time (action : unit -> 'T) =
        let sw = Stopwatch ()

        sw.Start ()

        let result = action ()
        
        sw.Stop ()

        sw.ElapsedMilliseconds, result

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
