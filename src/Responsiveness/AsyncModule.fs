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

    type FlowInterrupt =
        | FlowCancelled
        | Exception     of exn

    type FlowHandler = FlowInterrupt -> bool

    type FlowContinuation = int->unit

    [<NoComparison;Sealed>]
    type FlowExecutor(ctx : FlowContext) =

        let mutable handlers : FlowHandler list = []

        member x.Context = ctx

        member x.CheckCallingThread () =
            ctx.CheckCallingThread ()

        member x.Push (handler : FlowHandler) : unit =
            x.CheckCallingThread ()

            handlers <- handler::handlers

        member x.PushDisposable (d : IDisposable) : unit =
            x.Push (fun _ -> d.Dispose (); true)

        member x.PushFinallyHandler (handler : unit -> unit) : unit =
            x.Push (fun _ -> handler (); true)

        member x.PushWithHandler (handler : exn -> unit) : unit =
            x.Push (
                fun fi ->
                    match fi with
                    | FlowCancelled -> true
                    | Exception e   ->
                        handler e
                        false
                )

        member x.PopHandler () =
            x.CheckCallingThread ()

            match handlers with
            | _::hs -> handlers <- hs
            | _ -> failwith "Handler stack empty"

        member x.RaiseException (e : exn) : exn option =
            x.CheckCallingThread ()

            let rec raiseException (e : exn) (hs : FlowHandler list) : (exn option)*(FlowHandler list) =
                match hs with
                | []    -> (Some e),[]
                | h::hh ->
                    let result =
                        try
                            if h (Exception e) then
                                Some e
                            else
                                None

                        with
                        | e -> Some e

                    match result with
                    | None      -> None,hh
                    | Some ee   -> raiseException ee hh

            let oe,hs = raiseException e handlers

            handlers <- hs

            oe

        member x.CancelOperation () : exn list =
            x.CheckCallingThread ()

            let rec cancelOperation (exns : exn list) (hs : FlowHandler list) : exn list =
                match hs with
                | []    -> exns
                | h::hh ->
                    let result =
                        try
                            ignore <| h FlowCancelled
                            exns
                        with
                        | e -> e::exns

                    cancelOperation result hh

            let exns = cancelOperation []  handlers

            handlers <- []

            exns


    and [<NoEquality;NoComparison;Sealed>] FlowContext() =
        inherit BaseDisposable()

        let threadId        = Thread.CurrentThread.ManagedThreadId
        let continuations   = Dictionary<int, FlowExecutor*WaitHandle*FlowContinuation>()

        let mutable nextId  = 0
        let mutable waiting = false

        override x.OnDispose () =
            // TODO: Throw aggregate exception?
            ignore <| x.CancelExecution ()

        member x.CheckCallingThread () =
            let id = Thread.CurrentThread.ManagedThreadId
            if id <> threadId then failwithf "Wrong calling thread, expected: %d, actual: %d" threadId id

        member x.RegisterContinuation (exec : FlowExecutor) (waitHandle : WaitHandle) (continuation : FlowContinuation) : unit =
            x.CheckCallingThread ()

            let id = nextId
            nextId <- nextId + 1

            continuations.Add (id, (exec, waitHandle, continuation))

        member x.UnregisterContinuation (key : int) : unit =
            x.CheckCallingThread ()

            ignore <| continuations.Remove key

        member x.Continuations =
            x.CheckCallingThread ()

            [|
                for kv in continuations ->
                    let exec, waitHandle, continuation = kv.Value
                    kv.Key, exec, waitHandle, continuation
            |]

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
                                for _,_,waitHandle,_ in conts -> waitHandle
                            |]

                        let signaled = WaitHandle.WaitAny (waithandles)

                        let key,exec,_,continuation = conts.[signaled]

                        try
                            continuation key
                        with
                        | e ->
                            let oe = exec.RaiseException e
                            match oe with
                            | Some ee -> raise ee
                            | _ -> ()
                finally
                    waiting <- false

        member x.CancelExecution () : exn [] =
            x.CheckCallingThread ()

            let conts = x.Continuations

            continuations.Clear ()

            let execs = HashSet<_> (seq {for _,exec,_,_ in conts -> exec})

            execs
            |> Seq.map (fun exec -> exec.CancelOperation ())
            |> Seq.concat
            |> Seq.toArray

    type Continuation<'T> = 'T -> unit

    type Flow<'T> = FlowExecutor*Continuation<'T> -> unit

    module FlowModule =

        let Bind (t : Flow<'T>) (fu : 'T -> Flow<'U>) : Flow<'U> =
            fun (exec, cont) ->
                let tcont v =
                    exec.CheckCallingThread()
                    let u = fu v
                    u (exec, cont)

                t (exec, tcont)
        let Combine (t : Flow<unit>) (u : Flow<'T>) : Flow<'T> =
            fun (exec, cont) ->
                exec.CheckCallingThread()
                let tcont _ =
                    exec.CheckCallingThread()
                    u (exec, cont)

                t (exec, tcont)

        let Delay (dt : unit -> Flow<'T>) : Flow<'T> =
            fun (exec, cont) ->
                exec.CheckCallingThread()
                let t = dt ()
                t (exec, cont)

        let Return (v : 'T) : Flow<'T> =
            fun (exec, cont) ->
                exec.CheckCallingThread()
                cont v
        let ReturnFrom (t : Flow<'T>) : Flow<'T> = t
        let Zero : Flow<unit> =
            fun (exec, cont) ->
                exec.CheckCallingThread()
                cont ()

        let For (s : seq<'T>) (ft : 'T -> Flow<unit>) : Flow<unit> =
            fun (exec, cont) ->
                exec.CheckCallingThread()

                do
                    use e = s.GetEnumerator ()

                    let rec ic () =
                        if e.MoveNext () then
                            let t = ft e.Current
                            t (exec, ic)

                    ic ()

                cont ()
        let While (e : unit -> bool) (t : Flow<unit>) : Flow<unit> =
            fun (exec, cont) ->
                exec.CheckCallingThread()

                let rec ic () =
                    exec.CheckCallingThread()
                    if e () then
                        t (exec, ic)

                ic ()

                cont ()

        let Using (d : #IDisposable) (ft : #IDisposable -> Flow<'T>) : Flow<'T>=
            fun (exec, cont) ->
                exec.CheckCallingThread()

                let t = ft d

                let ic v =
                    exec.CheckCallingThread ()
                    exec.PopHandler ()
                    d.Dispose ()
                    cont v

                exec.PushDisposable d

                t (exec, ic)
        let TryFinally (t : Flow<'T>) (handler : unit->unit) : Flow<'T> =
            fun (exec, cont) ->
                exec.CheckCallingThread()

                let ic v =
                    exec.CheckCallingThread ()
                    exec.PopHandler ()
                    handler ()
                    cont v

                exec.PushFinallyHandler handler

                t (exec, ic)
        let TryWith (t : Flow<'T>) (fu : exn->Flow<'T>) : Flow<'T> =
            fun (exec, cont) ->
                exec.CheckCallingThread()

                let handler e =
                    let u = fu e
                    u (exec, cont)

                let ic v =
                    exec.CheckCallingThread ()
                    exec.PopHandler ()
                    cont v

                exec.PushWithHandler handler

                t (exec, ic)

    module Flow =

        let Run (t : Flow<'T>) : 'T =
            use ctx     = new FlowContext()
            let exec    = new FlowExecutor(ctx)

            let result = ref None
            let cont v = result := Some v

            try
                t (exec, cont)
            with
            | e ->
                let oe = exec.RaiseException e
                match oe with
                | Some ee -> raise ee
                | _ -> ()

            ctx.AwaitAllContinuations ()

            result.Value.Value

        let StartChild (t : Flow<'T>) : Flow<Flow<'T>> =
            fun (exec, cont) ->
                exec.CheckCallingThread()
                let rcont   = ref None
                let rvalue  = ref None

                let child : Flow<'T> =
                    fun (exec, cont) ->
                        exec.CheckCallingThread ()

                        rcont := Some cont
                        match !rvalue with
                        | Some v    -> cont v
                        | _         -> ()

                let icont v =
                    exec.CheckCallingThread ()

                    rvalue := Some v
                    match !rcont with
                    | Some c    -> c v
                    | _         -> ()

                let cexec = FlowExecutor(exec.Context)
                t (cexec, icont)

                cont child

        let AdaptWaitHandle (waitHandle : WaitHandle) : Flow<unit> =
            fun (exec, cont) ->
                exec.CheckCallingThread()

                let ic key =
                    exec.Context.UnregisterContinuation key

                    cont ()

                exec.Context.RegisterContinuation exec waitHandle ic

        let AdaptTask (t : Task<'T>) : Flow<'T> =
            fun (exec, cont) ->
                exec.CheckCallingThread()
                let inline tryRun () =
                    match t.Status with
                    | TaskStatus.Canceled           -> raise (OperationCanceledException ())
                    | TaskStatus.Faulted            -> raise t.Exception
                    | TaskStatus.RanToCompletion    -> cont t.Result; true
                    | _ -> false

                if tryRun () then ()
                else
                    let ar : IAsyncResult = upcast t

                    let ic key =
                        exec.Context.UnregisterContinuation key

                        let result = tryRun ()

                        if not result then failwith "Task execution failed"

                    exec.Context.RegisterContinuation exec ar.AsyncWaitHandle ic

        let AdaptUnitTask (t : Task) : Flow<unit> =
            fun (exec, cont) ->
                exec.CheckCallingThread()
                let inline tryRun () =
                    match t.Status with
                    | TaskStatus.Canceled           -> raise (OperationCanceledException ())
                    | TaskStatus.Faulted            -> raise t.Exception
                    | TaskStatus.RanToCompletion    -> cont (); true
                    | _ -> false

                if tryRun () then ()
                else
                    let ar : IAsyncResult = upcast t

                    let ic key =
                        exec.Context.UnregisterContinuation key

                        let result = tryRun ()

                        if not result then failwith "Task execution failed"

                    exec.Context.RegisterContinuation exec ar.AsyncWaitHandle ic


    type FlowBuilder() =

        member inline x.Bind(t,fu)      = FlowModule.Bind t fu
        member inline x.Combine(t,u)    = FlowModule.Combine t u

        member inline x.Delay(dt)       = FlowModule.Delay dt

        member inline x.Return(v)       = FlowModule.Return v
        member inline x.ReturnFrom(t)   = FlowModule.ReturnFrom t
        member inline x.Zero()          = FlowModule.Zero

        member inline x.For(s,ft)       = FlowModule.For s ft
        member inline x.While(g,t)      = FlowModule.While g t

        member inline x.Using(t,a)      = FlowModule.Using t a
        member inline x.TryFinally(t,a) = FlowModule.TryFinally t a
        member inline x.TryWith(t,a)    = FlowModule.TryWith t a

    let flow = FlowBuilder ()