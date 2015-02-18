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

module internal RStream =
    type RStream<'TIn, 'TOut> = ('TIn -> unit)*(unit->'TOut)

    let inline toSum (initialValue : 'T) : RStream<'T, 'T> =
        let sum = ref initialValue

        let inline ii v  = sum := !sum + v
        let inline oo () = !sum

        ii,oo

    let inline filter (p : 'TIn -> bool) ((i,o) : RStream<'TIn, 'TOut>) : RStream<'TIn, 'TOut> =

        let inline ii v  = if p v then i v

        ii,o

    let inline map (p : 'TIn -> 'U) ((i,o) : RStream<'U, 'TOut>) : RStream<'TIn, 'TOut> =

        let inline ii v  = i (p v)

        ii,o

    let inline ofRange (inclusiveBegin : 'TIn) (increment : 'TIn) (exclusiveEnd : 'TIn) ((i,o) : RStream<'TIn, 'TOut>) : 'TOut =

        let mutable iter = inclusiveBegin
        while iter < exclusiveEnd do
            i iter
            iter <- iter + increment

        o ()

    let inline ofArray (a : 'TIn []) ((i,o) : RStream<'TIn, 'TOut>) : 'TOut =

        for v in a do
            i v

        o ()


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
