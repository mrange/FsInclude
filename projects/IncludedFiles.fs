// @@@ INCLUDE: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Modules/BasicModule.fs
// @@@ BEGIN_DOCUMENT: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Modules/BasicModule.fs
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

namespace Included.FsInclude

module internal Opt =

    let inline valueOrDefault (defaultValue : 'T) (v : 'T option) : 'T =
        match v with
        | Some vv   -> vv
        | _         -> defaultValue

    let lift1
        (c  : 'T0 -> 'T )
        (v0 : 'T0 option)
        : 'T option =
        match v0 with
        | Some s0 -> Some <| c s0
        | _ -> None

    let lift2
        (c  : 'T0 -> 'T1 -> 'T )
        (v0 : 'T0 option)
        (v1 : 'T1 option)
        : 'T option =
        match v0, v1 with
        | Some s0, Some s1 -> Some <| c s0 s1
        | _ -> None

    let lift3
        (c  : 'T0 -> 'T1 -> 'T2 -> 'T )
        (v0 : 'T0 option)
        (v1 : 'T1 option)
        (v2 : 'T2 option)
        : 'T option =
        match v0, v1, v2 with
        | Some s0, Some s1, Some s2 -> Some <| c s0 s1 s2
        | _ -> None

    let lift4
        (c  : 'T0 -> 'T1 -> 'T2 -> 'T3 -> 'T )
        (v0 : 'T0 option)
        (v1 : 'T1 option)
        (v2 : 'T2 option)
        (v3 : 'T3 option)
        : 'T option =
        match v0, v1, v2, v3 with
        | Some s0, Some s1, Some s2, Some s3 -> Some <| c s0 s1 s2 s3
        | _ -> None

    let lift5
        (c  : 'T0 -> 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T )
        (v0 : 'T0 option)
        (v1 : 'T1 option)
        (v2 : 'T2 option)
        (v3 : 'T3 option)
        (v4 : 'T4 option)
        : 'T option =
        match v0, v1, v2, v3, v4 with
        | Some s0, Some s1, Some s2, Some s3, Some s4 -> Some <| c s0 s1 s2 s3 s4
        | _ -> None

module internal Numerical =

    let inline inRange v min max : bool =
        v >= min && v <= max

    let inline testRange v min max : int =
        if v < min then -1
        elif v > max then 1
        else 0

    let inline clamp v min max : 'T =
        if v < min then min
        elif v > max then max
        else v

    let inline lerp t min max : 'T =
        let zero = LanguagePrimitives.GenericZero
        let one  = LanguagePrimitives.GenericOne
        if t <= zero then min
        elif t >= one then max
        else t*(max - min) + min

    let inline mad x y z : 'T = x*y + z
// @@@ END_DOCUMENT: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Modules/BasicModule.fs
// @@@ INCLUDE: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Common/Disposable.fs
// @@@ BEGIN_DOCUMENT: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Common/Disposable.fs
// @@@ INCLUDE: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Common/Common.fs
// @@@ BEGIN_DOCUMENT: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Common/Common.fs
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

namespace Included.FsInclude

type internal LogLevel =
    | Info
    | Warning
    | Error
    | Exception

type internal ILog =
    interface
        abstract LogMessage: LogLevel*string -> unit
    end

module internal Log =

    open Microsoft.FSharp.Core.Printf
    open System.Text

    let empty =
        {
            new ILog with
                member x.LogMessage (ll, msg) =
                    let prelude =
                        match ll with
                        | Info      -> "INFO      : "
                        | Warning   -> "WARNING   : "
                        | Error     -> "ERROR     : "
                        | Exception -> "EXCEPTION : "
                    let sb = StringBuilder (prelude)
                    ignore <| sb.Append msg
                    System.Diagnostics.Trace.WriteLine <| sb.ToString ()
        }

    let mutable log = empty

    let info (message : string) : unit =
        log.LogMessage (LogLevel.Info, message)

    let infof (format : StringFormat<'T, unit>) : 'T =
        ksprintf info format

    let warning (message : string) : unit =
        log.LogMessage (LogLevel.Warning, message)

    let warningf (format : StringFormat<'T, unit>) : 'T =
        ksprintf warning format

    let error (message : string) : unit =
        log.LogMessage (LogLevel.Error, message)

    let errorf (format : StringFormat<'T, unit>) : 'T =
        ksprintf error format
// @@@ END_DOCUMENT: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Common/Common.fs

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

namespace Included.FsInclude

[<AbstractClass>]
type internal BaseDisposable() =
    [<DefaultValue>]
    val mutable isDisposed : int

    interface System.IDisposable with
        member x.Dispose () =
            if System.Threading.Interlocked.Exchange (&x.isDisposed, 1) = 0 then
                try
                    x.OnDispose ()
                with
                | e -> Log.errorf "%s.Dispose () threw exception: %A" (x.GetType().Name) e


    member x.IsDisposed = x.isDisposed <> 0

    member x.IsNotDisposed = x.isDisposed = 0

    member x.CheckDisposed () =
        if x.IsDisposed then raise (System.ObjectDisposedException (x.GetType().Name))

    abstract OnDispose: unit -> unit

type internal ActionDisposable(action : unit -> unit) =
    inherit BaseDisposable()

    override x.OnDispose () = action ()

module internal Disposable =
    let onExitDo (action : unit -> unit) : System.IDisposable = upcast new ActionDisposable(action)

    let inline dispose (d : System.IDisposable) =
        if d <> null then d.Dispose ()

    let noThrowDispose (d : System.IDisposable) =
        if d <> null then
            try
                d.Dispose ()
            with
            | e ->
                Log.errorf "%s.Dispose () threw exception: %A" (d.GetType().Name) e

// @@@ END_DOCUMENT: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Common/Disposable.fs
// @@@ INCLUDE: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Responsiveness/MultiplexModule.fs
// @@@ BEGIN_DOCUMENT: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Responsiveness/MultiplexModule.fs
// @@@ SKIPPED_INCLUDE (Already seen): https://raw.githubusercontent.com/mrange/FsInclude/master/src/Common/Disposable.fs
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

namespace Included.FsInclude

module internal Multiplex =

    open System
    open System.Collections.Generic
    open System.Diagnostics
    open System.Threading
    open System.Threading.Tasks

    module Details =
        type FlowInterrupt =
            | FlowCancelled
            | Exception     of exn

        type FlowHandler = FlowInterrupt -> bool

        type FlowContinuation = int->unit

        [<NoEquality;NoComparison;Sealed>]
        type FlowExecutor(ctx : FlowContext) as x =

            let mutable unprotected_handlers : FlowHandler list = []

            do
                ctx.AddExecutor x

            member x.Context = ctx

            member x.CheckCallingThread () =
                ctx.CheckCallingThread ()

            member x.Push (handler : FlowHandler) : unit =
                x.CheckCallingThread ()

                if ctx.IsDisposed then ()   // TODO: Trace?
                else
                    // unprotected access ok as we checked calling thread
                    unprotected_handlers <- handler::unprotected_handlers

            member x.PushDisposable (d : IDisposable) : unit =
                x.Push (fun _ -> Disposable.dispose d; true)

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

                // unprotected access ok as we checked calling thread
                match unprotected_handlers with
                | _::hs -> unprotected_handlers <- hs
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

                // unprotected access ok as we checked calling thread
                let oe,hs = raiseException e unprotected_handlers

                unprotected_handlers <- hs

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

                // unprotected access ok as we checked calling thread
                let exns = cancelOperation [] unprotected_handlers

                unprotected_handlers <- []

                exns


        and [<NoEquality;NoComparison;Sealed>] FlowContext() =
            inherit BaseDisposable()

            let threadId                    = Thread.CurrentThread.ManagedThreadId
            let unprotected_continuations   = Dictionary<int, FlowExecutor*WaitHandle*FlowContinuation>()

            let mutable unprotected_nextId  = 0

            let unprotected_executors       = ResizeArray<FlowExecutor>()

            let unprotected_hasContinuations () = unprotected_continuations.Count > 0

            let unprotected_flatContinuations ()=
                [|
                    for kv in unprotected_continuations ->
                        let exec, waitHandle, continuation = kv.Value
                        kv.Key, exec, waitHandle, continuation
                |]


            override x.OnDispose () =
                // TODO: Throw aggregate exception?
                ignore <| x.CancelExecution ()

            member x.CheckCallingThread () =
                let id = Thread.CurrentThread.ManagedThreadId
                if id <> threadId then failwithf "Wrong calling thread, expected: %d, actual: %d" threadId id

            member x.RegisterContinuation (exec : FlowExecutor) (waitHandle : WaitHandle) (continuation : FlowContinuation) : unit =
                x.CheckCallingThread ()

                // unprotected access ok as we checked calling thread
                let id = unprotected_nextId
                unprotected_nextId <- unprotected_nextId + 1

                // unprotected access ok as we checked calling thread
                unprotected_continuations.Add (id, (exec, waitHandle, continuation))

            member x.UnregisterContinuation (key : int) : unit =
                x.CheckCallingThread ()

                // unprotected access ok as we checked calling thread
                ignore <| unprotected_continuations.Remove key

            member x.UnregisterAllMatchingContinuations (ctx : FlowContext) : unit =
                x.CheckCallingThread ()

                // unprotected access ok as we checked calling thread
                let toBeRemoved =
                    unprotected_continuations
                    |> Seq.filter (fun kv -> let exec,_,_ = kv.Value in obj.ReferenceEquals (exec.Context, ctx))
                    |> Seq.toArray

                // unprotected access ok as we checked calling thread
                if toBeRemoved.Length > 0 then
                    for kv in toBeRemoved do
                        ignore <| unprotected_continuations.Remove kv.Key

            member x.HasContinuations =
                x.CheckCallingThread ()

                // unprotected access ok as we checked calling thread
                unprotected_continuations.Count > 0

            member x.Continuations =
                x.CheckCallingThread ()

                // unprotected access ok as we checked calling thread
                unprotected_flatContinuations ()

            member x.AwaitContinuations (exe : exn -> unit) : bool =
                x.CheckCallingThread ()

                // unprotected access ok as we checked calling thread
                if not (unprotected_hasContinuations ()) then false
                else
                    let flat = unprotected_flatContinuations ()

                    let waithandles =
                        [|
                            for _,_,waitHandle,_ in flat -> waitHandle
                        |]

                    let signaled = WaitHandle.WaitAny waithandles

                    let key,exec,_,continuation = flat.[signaled]

                    try
                        continuation key
                    with
                    | e ->
                        let oe = exec.RaiseException e
                        match oe with
                        | Some ee -> exe ee
                        | _ -> ()

                    // unprotected access ok as we checked calling thread
                    unprotected_hasContinuations ()

            member x.AwaitAllContinuations (exe : exn -> unit) : unit =
                while x.AwaitContinuations exe do
                    ()

            member x.AddExecutor (exec : FlowExecutor) : unit =
                x.CheckCallingThread ()

                // unprotected access ok as we checked calling thread
                unprotected_executors.Add exec

            member x.CancelExecution () : exn [] =
                x.CheckCallingThread ()

                let exns = ResizeArray<exn> ()

                // unprotected access ok as we checked calling thread
                let execs= unprotected_executors.ToArray ()

                unprotected_executors.Clear ()

                x.UnregisterAllMatchingContinuations x

                for exec in execs do
                    exns.AddRange (exec.CancelOperation ())

                exns.ToArray ()

        type Continuation<'T> = 'T -> unit

    open Details

    type Flow<'T> = FlowExecutor*Continuation<'T> -> unit

    module ComputationExpression =
        module FlowModule =

            let bind (t : Flow<'T>) (fu : 'T -> Flow<'U>) : Flow<'U> =
                fun (exec, cont) ->
                    exec.CheckCallingThread()
                    let tcont v =
                        exec.CheckCallingThread()
                        let u = fu v
                        u (exec, cont)

                    t (exec, tcont)
            let combine (t : Flow<unit>) (u : Flow<'T>) : Flow<'T> =
                fun (exec, cont) ->
                    exec.CheckCallingThread()
                    let tcont _ =
                        exec.CheckCallingThread()
                        u (exec, cont)

                    t (exec, tcont)

            let delay (dt : unit -> Flow<'T>) : Flow<'T> =
                fun (exec, cont) ->
                    exec.CheckCallingThread()
                    let t = dt ()
                    t (exec, cont)

            let returnValue (v : 'T) : Flow<'T> =
                fun (exec, cont) ->
                    exec.CheckCallingThread()
                    cont v
            let returnFrom (t : Flow<'T>) : Flow<'T> = t
            let zero : Flow<unit> =
                fun (exec, cont) ->
                    exec.CheckCallingThread()
                    cont ()

            let forEach (s : seq<'T>) (ft : 'T -> Flow<unit>) : Flow<unit> =
                fun (exec, cont) ->
                    exec.CheckCallingThread()

                    do
                        let e = s.GetEnumerator ()
                        exec.PushDisposable e
                        let rec ic () =
                            if e.MoveNext () then
                                let t = ft e.Current
                                t (exec, ic)
                            else
                                exec.PopHandler ()
                                Disposable.dispose e
                                cont ()

                        ic ()

            let whileDo (e : unit -> bool) (t : Flow<unit>) : Flow<unit> =
                fun (exec, cont) ->
                    exec.CheckCallingThread()

                    let rec ic () =
                        exec.CheckCallingThread()
                        if e () then
                            t (exec, ic)
                        else
                            cont ()

                    ic ()

            let using (d : #IDisposable) (ft : #IDisposable -> Flow<'T>) : Flow<'T>=
                fun (exec, cont) ->
                    exec.CheckCallingThread()

                    exec.PushDisposable d

                    let t = ft d

                    let ic v =
                        exec.CheckCallingThread ()
                        exec.PopHandler ()
                        Disposable.dispose d
                        cont v

                    t (exec, ic)
            let tryFinally (t : Flow<'T>) (handler : unit->unit) : Flow<'T> =
                fun (exec, cont) ->
                    exec.CheckCallingThread()

                    exec.PushFinallyHandler handler

                    let ic v =
                        exec.CheckCallingThread ()
                        exec.PopHandler ()
                        handler ()
                        cont v


                    t (exec, ic)
            let tryWith (t : Flow<'T>) (fu : exn->Flow<'T>) : Flow<'T> =
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

        type FlowBuilder() =

            member inline x.Bind(t,fu)      = FlowModule.bind t fu
            member inline x.Combine(t,u)    = FlowModule.combine t u

            member inline x.Delay(dt)       = FlowModule.delay dt

            member inline x.Return(v)       = FlowModule.returnValue v
            member inline x.ReturnFrom(t)   = FlowModule.returnFrom t
            member inline x.Zero()          = FlowModule.zero

            member inline x.For(s,ft)       = FlowModule.forEach s ft
            member inline x.While(g,t)      = FlowModule.whileDo g t

            member inline x.Using(t,a)      = FlowModule.using t a
            member inline x.TryFinally(t,a) = FlowModule.tryFinally t a
            member inline x.TryWith(t,a)    = FlowModule.tryWith t a

    open ComputationExpression

    let flow = FlowBuilder ()

    module Flow =

        let run (t : Flow<'T>) : 'T =
            use ctx     = new FlowContext()
            let exec    = FlowExecutor(ctx)

            let result = ref None

            let cont v  = result := Some v
            let exe ex  = raise ex

            try
                t (exec, cont)
                ctx.AwaitAllContinuations exe
            with
            | e ->
                let oe = exec.RaiseException e
                match oe with
                | Some ee -> exe ee
                | _ -> ctx.AwaitAllContinuations exe

            result.Value.Value

        let startChild (t : Flow<'T>) : Flow<Flow<'T>> =
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

        let adaptWaitHandle (waitHandle : WaitHandle) : Flow<unit> =
            fun (exec, cont) ->
                exec.CheckCallingThread()

                let ic key =
                    exec.Context.UnregisterContinuation key

                    cont ()

                exec.Context.RegisterContinuation exec waitHandle ic

        let adaptLegacyAsync (ar : IAsyncResult) (endAsync : IAsyncResult->'T) : Flow<'T> =
            fun (exec, cont) ->
                exec.CheckCallingThread ()

                let ic key =
                    exec.Context.UnregisterContinuation key

                    cont (endAsync ar)

                exec.Context.RegisterContinuation exec ar.AsyncWaitHandle ic

        let adaptTask (t : Task<'T>) : Flow<'T> =
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

        let adaptUnitTask (t : Task) : Flow<unit> =
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
// @@@ END_DOCUMENT: https://raw.githubusercontent.com/mrange/FsInclude/master/src/Responsiveness/MultiplexModule.fs
namespace Included
module IncludeMetaData =
    [<Literal>]
    let IncludeDate = "2015-02-18T23:11:29"
    [<Literal>]
    let Include_0 = @"https://raw.githubusercontent.com/mrange/FsInclude/master/src/Modules/BasicModule.fs"
    [<Literal>]
    let Include_1 = @"https://raw.githubusercontent.com/mrange/FsInclude/master/src/Common/Disposable.fs"
    [<Literal>]
    let Include_2 = @"https://raw.githubusercontent.com/mrange/FsInclude/master/src/Common/Common.fs"
    [<Literal>]
    let Include_3 = @"https://raw.githubusercontent.com/mrange/FsInclude/master/src/Responsiveness/MultiplexModule.fs"

