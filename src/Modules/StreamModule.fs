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

type RReceiver<'T>  = 'T -> bool 
type RStream<'T>    = RReceiver<'T> -> unit

module RStream =

    let inline filter (f : 'T -> bool) (s : RStream<'T>) : RStream<'T> = 
        fun receiver ->
            let r v = 
                if (f v) then receiver v
                else true
            s r

    let inline map (f : 'T -> 'U) (s : RStream<'T>) : RStream<'U> = 
        fun receiver ->
            let r v = receiver (f v)
            s r

    let inline ofType (s : RStream<'T>) : RStream<'U> = 
        fun receiver ->
            let r (v : 'T) =
                match box v with
                | :? 'U as u -> receiver u
                | _ -> true
            s r

    let inline take (n : int) (s : RStream<'T>) : RStream<'T> = 
        fun receiver ->
            let rn = ref n
            let r v = 
                if !rn > 0 then
                    rn := !rn - 1
                    receiver v
                else
                    false
            s r

    let inline skip (n : int) (s : RStream<'T>) : RStream<'T> = 
        fun receiver ->
            let rn = ref n
            let r v = 
                if !rn > 0 then
                    rn := !rn - 1
                    true
                else
                    receiver v
            s r

    let inline toArray (s : RStream<'T>) : 'T [] = 
        let ra = ResizeArray<_> ()
        let r v = ra.Add v; true
        s r
        ra.ToArray ()

    let inline toList (s : RStream<'T>) : 'T list = 
        let l = ref List.empty
        let r v = l := v::!l; true
        s r
        List.rev !l

    let inline toSum (initial : 'T) (s : RStream<'T>) : 'T = 
        let sum = ref initial
        let r v = sum := !sum + v; true
        s r
        !sum

    let inline ofRange (f : 'T) (inc : 'T) (t : 'T) : RStream<'T> =
        fun receiver ->
            let mutable i = f
            while i < t && (receiver i) do
                i <- i + inc

    let inline ofArray (a : 'T []) : RStream<'T> =
        fun receiver ->
            let l = a.Length
            let mutable i = 0
            while i < l && (receiver a.[i]) do
                i <- i + 1

    let inline ofList (l : 'T list) : RStream<'T> =
        fun receiver ->
            let mutable i = l
            while i.IsEmpty && (receiver i.Head) do
                i <- i.Tail

    let inline ofSeq (l : seq<'T>) : RStream<'T> =
        fun receiver ->
            use e = l.GetEnumerator ()
            while e.MoveNext () && (receiver e.Current) do
                ()

    let inline firstOrValue (v : 'T) (s : RStream<'T>) : 'T =
        let rv = ref v
        let a v =
            rv := v
            false
        s a
        !rv

    let inline first (s : RStream<'T>) : 'T option =
        let rv = ref None
        let a v =
            rv := Some v
            false
        s a
        !rv


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
