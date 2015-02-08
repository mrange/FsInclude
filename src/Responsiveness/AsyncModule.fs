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

module internal Gem =

    open System
    open System.Collections.Generic
    open System.Diagnostics
    open System.Threading
    open System.Threading.Tasks


    type FlowContinuationResult =
        | Success
        | ApplicationShutDown
        | ThreadShutDown
        | UnrecoverableErrorDetected    of exn
        | TokenCancelled                of CancellationToken
        | UserCancelled                 of obj


    type FlowContext() =
        inherit BaseDisposable()

        let threadId        = Thread.CurrentThread.ManagedThreadId
        let continuations   = Dictionary<int, WaitHandle*(int*FlowContinuationResult->unit)>()

        let mutable nextId  = 0
        let mutable waiting = false

        override x.OnDispose () = ()

        member x.CheckCallingThread () =
            let id = Thread.CurrentThread.ManagedThreadId
            if id <> threadId then failwithf "Wrong calling thread, expected: %d, actual: %d" threadId id

        member x.RegisterContinuation (waitHandle : WaitHandle) (continuation : int*FlowContinuationResult->unit) : unit =
            x.CheckCallingThread ()

            let id = nextId
            nextId <- nextId + 1

            continuations.Add (id, (waitHandle, continuation))

        member x.UnregisterWaitHandle (key : int) : unit =
            x.CheckCallingThread ()

            ignore <| continuations.Remove key

        member x.Continuations =
            x.CheckCallingThread ()

            [|
                for kv in continuations -> 
                    let waitHandle, continuation = kv.Value
                    kv.Key, waitHandle, continuation
            |]

        member x.IsAwaiting = 
            x.CheckCallingThread ()
            waiting

        member x.AwaitAllContinuations () : unit =
            x.CheckCallingThread ()

            if waiting then ()
            else
                waiting <- true
                try
                    while continuations.Count > 0 do
                        let continuations = x.Continuations

                        let waithandles =
                            [|
                                for _,waitHandle,_ in continuations -> waitHandle
                            |]

                        // TODO: Handle different error cases
                        let signaled = WaitHandle.WaitAny (waithandles)

                        let key,_,continuation = continuations.[signaled]

                        continuation (key, Success)
                finally
                    waiting <- false


        member x.RaiseException (e : exn) = ()
        member x.CancelOperation () = ()
(* 
        member x.IsAwaiting =
 
        member x.CancelAllContinuations (cr : CancelReason) : unit =
 
        member x.AwaitAllContinuations () : unit =
        
*)

    type Continuation<'T> = 'T -> unit

    type Flow<'T> = FlowContext*Continuation<'T> -> unit

    module FlowModule =

        let Return (v : 'T) : Flow<'T> =
            fun (ctx, cont) ->
                cont v

        let Bind (t : Flow<'T>) (fu : 'T -> Flow<'U>) : Flow<'U> =
            fun (ctx, cont) ->
                let tcont v =
                    let u = fu v
                    u (ctx, cont)
                
                t (ctx, tcont)

        let Yield (dt : unit -> Flow<'T>) : Flow<'T> =
            fun (ctx, cont) ->
                let t = dt ()
                t (ctx, cont)

    module Flow =

        let Run (t : Flow<'T>) : 'T =
            use ctx = new FlowContext()

            let result = ref Unchecked.defaultof<'T>
            let cont v = result := v

            t (ctx, cont)

            ctx.AwaitAllContinuations ()

            !result

        let StartChild (t : Flow<'T>) : Flow<Flow<'T>> =
            fun (ctx, cont) ->
                let rcont   = ref None
                let rvalue  = ref None

                let child : Flow<'T> =
                    fun (ctx, cont) ->
                        ctx.CheckCallingThread ()

                        rcont := Some cont
                        match !rvalue with
                        | Some v    -> cont v
                        | _         -> ()

                let icont v =
                    ctx.CheckCallingThread ()

                    rvalue := Some v
                    match !rcont with
                    | Some c    -> c v
                    | _         -> ()

                t (ctx, icont)

                cont child

        let AdaptTask (t : Task<'T>) : Flow<'T> =
            fun (ctx, cont) ->
                let inline tryRun () = 
                    match t.Status with
                    | TaskStatus.Canceled           -> ctx.RaiseException t.Exception   ; true
                    | TaskStatus.Faulted            -> ctx.CancelOperation ()           ; true
                    | TaskStatus.RanToCompletion    -> cont t.Result                    ; true
                    | _ -> false

                if tryRun () then ()
                else
                    let ar : IAsyncResult = upcast t

                    let ic (key : int, fcr : FlowContinuationResult) =
                        ctx.UnregisterWaitHandle key

                        let result = tryRun ()

                        if not result then failwith "Task execution failed"

                    ctx.RegisterContinuation ar.AsyncWaitHandle ic

        let AdaptUnitTask (t : Task) : Flow<unit> =
            fun (ctx, cont) ->
                let inline tryRun () = 
                    match t.Status with
                    | TaskStatus.Canceled           -> ctx.RaiseException t.Exception   ; true
                    | TaskStatus.Faulted            -> ctx.CancelOperation ()           ; true
                    | TaskStatus.RanToCompletion    -> cont ()                          ; true
                    | _ -> false

                if tryRun () then ()
                else
                    let ar : IAsyncResult = upcast t

                    let ic (key : int, fcr : FlowContinuationResult) =
                        ctx.UnregisterWaitHandle key

                        let result = tryRun ()

                        if not result then failwith "Task execution failed"

                    ctx.RegisterContinuation ar.AsyncWaitHandle ic
            
    
    type FlowBuilder() =

        member x.Return(v)      = FlowModule.Return v
        member x.Bind(t,fu)     = FlowModule.Bind t fu
        member x.Yield(dt)      = FlowModule.Yield dt

    let flow = FlowBuilder ()