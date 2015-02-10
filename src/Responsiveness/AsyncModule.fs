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
        | FlowCancelled                 
        | TokenCancelled                of CancellationToken
        | UserCancelled                 of obj
        | UnrecoverableErrorDetected    of exn
        | ThreadShutDown
        | ApplicationShutDown
    
    type FlowContext() =
        inherit BaseDisposable()

        let threadId        = Thread.CurrentThread.ManagedThreadId
        let continuations   = Dictionary<int, WaitHandle*(int*FlowContinuationResult->unit)>()

        let mutable nextId  = 0
        let mutable waiting = false

        override x.OnDispose () = 
            x.CancelAllContinuations FlowCancelled

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
                        let conts = x.Continuations

                        let waithandles =
                            [|
                                for _,waitHandle,_ in conts -> waitHandle
                            |]

                        // TODO: Handle different error cases
                        let signaled = WaitHandle.WaitAny (waithandles)

                        let key,_,continuation = conts.[signaled]

                        continuation (key, Success)
                finally
                    waiting <- false

        member x.CancelAllContinuations (fcr : FlowContinuationResult) : unit =
            x.CheckCallingThread ()

            let conts = x.Continuations
            
            continuations.Clear ()

            for key,_,continuation in conts do
                try
                    continuation (key, fcr)
                with 
                | e -> 
                    ()  // TODO: Accumulate exceptions

    type Continuation<'T> = 'T -> unit

    type Flow<'T> = FlowContext*Continuation<'T> -> unit

    module FlowModule =

        let Return (v : 'T) : Flow<'T> =
            fun (ctx, cont) ->
                ctx.CheckCallingThread()
                cont v

        let ReturnFrom (t : Flow<'T>) : Flow<'T> = t

        let Bind (t : Flow<'T>) (fu : 'T -> Flow<'U>) : Flow<'U> =
            fun (ctx, cont) ->
                let tcont v =
                    ctx.CheckCallingThread()
                    let u = fu v
                    u (ctx, cont)
                
                t (ctx, tcont)

        let Combine (t : Flow<unit>) (u : Flow<'T>) : Flow<'T> =
            fun (ctx, cont) ->
                ctx.CheckCallingThread()
                let tcont _ =
                    ctx.CheckCallingThread()
                    u (ctx, cont)
                
                t (ctx, tcont)

        let Delay (dt : unit -> Flow<'T>) : Flow<'T> =
            fun (ctx, cont) ->
                ctx.CheckCallingThread()
                let t = dt ()
                t (ctx, cont)

        let For (s : seq<'T>) (ft : 'T -> Flow<unit>) : Flow<unit> =
            fun (ctx, cont) ->
                ctx.CheckCallingThread()

                do
                    use e = s.GetEnumerator ()

                    let rec ic () = 
                        if e.MoveNext () then
                            let t = ft e.Current
                            t (ctx, ic)

                    ic ()

                cont ()

        // TODO: All these exception handlers might need be stored in context to support
        // calling all finalizers on exception

        let Using (v : #IDisposable) (ft : #IDisposable -> Flow<'T>) : Flow<'T>= 
            fun (ctx, cont) ->
                ctx.CheckCallingThread()
                let t = ft v

                let rv = ref None
                let ic v =
                    ctx.CheckCallingThread ()
                    rv := Some v
                try
                    t (ctx, ic)
                finally
                    v.Dispose ()

                cont (!rv).Value

        let TryFinally (t : Flow<'T>) (handler : unit->unit) : Flow<'T> = 
            fun (ctx, cont) ->
                ctx.CheckCallingThread()
                let rv = ref None
                let ic v =
                    ctx.CheckCallingThread ()
                    rv := Some v
                try
                    t (ctx, ic)
                finally
                    handler ()

                cont (!rv).Value

        let TryWith (t : Flow<'T>) (handler : exn->Flow<'T>) : Flow<'T> = 
            fun (ctx, cont) ->
                ctx.CheckCallingThread()
                let rv = ref None
                let re = ref None

                let ic v = 
                    ctx.CheckCallingThread ()
                    rv := Some v
                try
                    t (ctx, ic)
                with
                | e ->
                    re := Some e

                match !rv, !re with
                | Some v, None  -> cont v
                | None  , Some e-> 
                    let u = handler e
                    u (ctx, cont) 
                | _     , _     -> failwith "Invalid case" // TODO:

        let While (e : unit -> bool) (t : Flow<unit>) : Flow<unit> =
            fun (ctx, cont) ->
                ctx.CheckCallingThread()

                let rec ic () = 
                    ctx.CheckCallingThread()
                    if e () then
                        t (ctx, ic)

                ic ()

                cont ()

        let Zero : Flow<unit> =
            fun (ctx : FlowContext, cont) ->
                ctx.CheckCallingThread()
                cont ()

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
                ctx.CheckCallingThread()
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
                ctx.CheckCallingThread()
                let inline tryRun () = 
                    match t.Status with
                    | TaskStatus.Canceled           -> raise (OperationCanceledException ())
                    | TaskStatus.Faulted            -> raise t.Exception
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
                ctx.CheckCallingThread()
                let inline tryRun () = 
                    match t.Status with
                    | TaskStatus.Canceled           -> raise (OperationCanceledException ())
                    | TaskStatus.Faulted            -> raise t.Exception
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

        member inline x.Bind(t,fu)      = FlowModule.Bind t fu
        member inline x.Combine(t,u)    = FlowModule.Combine t u
        member inline x.Delay(dt)       = FlowModule.Delay dt
        member inline x.For(s,ft)       = FlowModule.For s ft
        member inline x.Using(t,a)      = FlowModule.Using t a
        member inline x.Return(v)       = FlowModule.Return v
        member inline x.ReturnFrom(t)   = FlowModule.ReturnFrom t
        member inline x.TryFinally(t,a) = FlowModule.TryFinally t a
        member inline x.TryWith(t,a)    = FlowModule.TryWith t a
        member inline x.While(g,t)      = FlowModule.While g t
        member inline x.Zero()          = FlowModule.Zero

    let flow = FlowBuilder ()