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

module Opt =

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

module Streams =

    type Stream<'T> = ('T -> bool) -> unit

    let filter (f : 'T -> bool) (s : Stream<'T>) : Stream<'T> = 
        fun action ->
            let a v = 
                if (f v) then action v
                else true
            s a

    let map (f : 'T -> 'U) (s : Stream<'T>) : Stream<'U> = 
        fun action ->
            let a v = action (f v)
            s a

    let toArray (s : Stream<'T>) : 'T [] = 
        let ra = ResizeArray<_> ()
        let a v = ra.Add v; true
        s a
        ra.ToArray ()

    let toList (s : Stream<'T>) : 'T list = 
        let l = ref List.empty
        let a v = l := v::!l; true
        s a
        List.rev !l

    let inline sum (initial : 'T) (s : Stream<'T>) : 'T = 
        let sum = ref initial
        let a v = sum := !sum + v; true
        s a
        !sum

    let inline fromRange (f : 'T) (inc : 'T) (t : 'T) : Stream<'T> =
        fun action ->
            let mutable i = f
            while i < t && (action i) do
                i <- i + inc

    let fromArray (a : 'T []) : Stream<'T> =
        fun action ->
            let l = a.Length
            let mutable i = 0
            while i < l && (action a.[i]) do
                i <- i + 1

    let fromList (l : 'T list) : Stream<'T> =
        fun action ->
            let mutable i = l
            while i.IsEmpty && (action i.Head) do
                i <- i.Tail
