// Copyright 2015 M�rten R�nge
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

module Stream =

    type Context() =
        member val IsCancellable    = false with get, set
        member val Continue         = true  with get, set

    type Receiver<'T>   = Context*('T -> unit)
    type Stream<'T>     = Receiver<'T> -> unit


    let inline filter (f : 'T -> bool) (s : Stream<'T>) : Stream<'T> =
        fun (context,receiver) ->
            let inline r v =
                if (f v) then receiver v
            s (context,r)

    let inline map (f : 'T -> 'U) (s : Stream<'T>) : Stream<'U> =
        fun (context,receiver) ->
            let inline r v = receiver (f v)
            s (context,r)

    let inline ofType (s : Stream<'T>) : Stream<'U> =
        fun (context,receiver) ->
            let inline r (v : 'T) =
                match box v with
                | :? 'U as u -> receiver u
                | _ -> ()
            s (context,r)

    let inline take (n : int) (s : Stream<'T>) : Stream<'T> =
        fun (context,receiver) ->
            context.IsCancellable <- true
            let rn = ref n
            let inline r v =
                if !rn >= 0 then
                    rn := !rn - 1
                    receiver v
                else
                    context.Continue <- false
            s (context,r)

    let inline skip (n : int) (s : Stream<'T>) : Stream<'T> =
        fun (context,receiver) ->
            let rn = ref n
            let inline r v =
                if !rn > 0 then
                    rn := !rn - 1
                else
                    receiver v
            s (context,r)

    let inline toArray (s : Stream<'T>) : 'T [] =
        let ra = ResizeArray<_> ()
        let c = Context()
        let inline r v = ra.Add v
        s (c,r)
        ra.ToArray ()

    let inline toList (s : Stream<'T>) : 'T list =
        let l = ref List.empty
        let c = Context()
        let inline r v = l := v::!l
        s (c,r)
        List.rev !l

    let inline toSum (initial : 'T) (s : Stream<'T>) : 'T =
        let sum = ref initial
        let c = Context()
        let inline r v = sum := !sum + v
        s (c,r)
        !sum

    let inline ofRange (inclusiveBegin : 'T) (increment : 'T) (exclusiveEnd : 'T) : Stream<'T> =
        fun (context,receiver) ->
            if context.IsCancellable then
                let mutable i = inclusiveBegin
                while i < exclusiveEnd && (receiver i; context.Continue) do
                    i <- i + increment
            else
                let mutable i = inclusiveBegin
                while i < exclusiveEnd do
                    receiver i
                    i <- i + increment

    let inline ofArray (a : 'T []) : Stream<'T> =
        fun (context,receiver) ->
            if context.IsCancellable then
                let l = a.Length
                let mutable i = 0
                while i < l && (receiver a.[i]; context.Continue) do
                    i <- i + 1
            else
                for v in a do
                    receiver v

    let inline ofList (l : 'T list) : Stream<'T> =
        fun (context,receiver) ->
            if context.IsCancellable then
                let mutable i = l
                while i.IsEmpty && (receiver i.Head; context.Continue) do
                    i <- i.Tail
            else
                let mutable i = l
                while i.IsEmpty do
                    receiver i.Head
                    i <- i.Tail


    let inline ofSeq (l : seq<'T>) : Stream<'T> =
        fun (context,receiver) ->
            if context.IsCancellable then
                use e = l.GetEnumerator ()
                while e.MoveNext () && (receiver e.Current; context.Continue) do
                    ()
            else
                use e = l.GetEnumerator ()
                while e.MoveNext () do
                    receiver e.Current

(*

type FReceiver<'T> =
    interface
        abstract Receive: 'T -> bool
    end

type FStream<'T> = FReceiver<'T> -> unit

module FStream =

    let inline filter (f : 'T -> bool) (s : FStream<'T>) : FStream<'T> =
        fun receiver ->
            let r =
                {
                    new FReceiver<'T> with
                        override x.Receive v =
                            if (f v) then receiver.Receive v
                            else true
                }
            s r

    let inline map (f : 'T -> 'U) (s : FStream<'T>) : FStream<'U> =
        fun receiver ->
            let r =
                {
                    new FReceiver<'T> with
                        override x.Receive v = receiver.Receive (f v)
                }
            s r

    let inline ofArray (a : 'T []) : FStream<'T> =
        fun receiver ->
            let l = a.Length
            let mutable i = 0
            while i < l && (receiver.Receive a.[i]) do
                i <- i + 1

    let inline ofRange (f : 'T) (inc : 'T) (t : 'T) : FStream<'T> =
        fun receiver ->
            let mutable i = f
            while i < t && receiver.Receive i do
                i <- i + inc


    let inline toSum (initial : 'T) (s : FStream<'T>) : 'T =
        let sum = ref initial
        let r =
            {
                new FReceiver<'T> with
                    override x.Receive v =
                        sum := !sum + v
                        true
            }

        s r
        !sum
*)