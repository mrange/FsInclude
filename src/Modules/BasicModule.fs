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

module Numerical =

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
